

## ------ common functions

#' Create new database object with validation and augmentation
#'
#' Takes a partial object specification, fills in missing fields with NA, generates
#' IDs and metadata, then runs validation and augmentation functions. This is the
#' entry point for all object creation in ErudicionDB.
#'
#' @param connection Database connection or pool
#' @param object_type Type of object (singular form: "item", "person", etc.)
#' @param object List containing object data (partial or complete)
#' @param validate_function Optional validation function to check object integrity
#' @param augment_function Optional augmentation function to enrich object data
#' @param stage Activation stage (0 = active, -1 = inactive, default: 0)
#'
#' @return List containing the complete, validated, and augmented object with:
#'   - All table columns present (missing fields filled with NA)
#'   - Generated UUID for object_id
#'   - object_type field set
#'   - revision set to 1
#'   - stage set to specified value
#'
#' @details
#' The function follows the validator-augmentor pattern:
#' 1. Validates all provided fields exist in the target table schema
#' 2. Fills missing fields with NA based on table schema
#' 3. Adds metadata: object_type, UUID, revision (1), stage
#' 4. Runs validator function (if provided) - throws error on failure
#' 5. Runs augmentor function (if provided) - transforms/enriches data
#'
#' @note
#' Object is NOT inserted into database - use `.insert_one()` or
#' `.insert_new_object()` for that. The object_type field must match a valid
#' table name (pluralized: "items", "persons", etc.).
#'
#' @keywords internal
.new_object <- function(connection, object_type, object,
                        validate_function=NULL,
                        augment_function=NULL,
                        stage=0) {
  object_id_name <- glue::glue("{object_type}_id")
  object_table_name <- glue::glue("{object_type}s")
  object_table_columns <- DBI::dbListFields(connection, object_table_name)
  for (field in names(object)) {
    assertthat::assert_that(field %in% object_table_columns,
                            msg=glue::glue("unknown {object_type} field '{field}'"))
  }
  for (field in object_table_columns) {
    if (!(field %in% names(object)) || is.null(object[[field]])) {
      object[[field]] <- NA
    }
  }

  object$object_type <- object_type
  object[[object_id_name]] <- uuid::UUIDgenerate()

  object$revision <- 1
  object$stage <- stage

  if (!is.null(validate_function) && is.function(validate_function)) {
    validate_function(object)
  }

  if (!is.null(augment_function) && is.function(augment_function)) {
    return(augment_function(object))
  } else {
    return(object)
  }
}

#' Calculate next revision number for an object
#'
#' Finds the maximum existing revision number for an object and returns the next
#' sequential revision number. Used when creating new revisions during updates.
#'
#' @param connection Database connection or pool
#' @param object_type Type of object (singular form: "item", "person", etc.)
#' @param object_id UUID of the object
#'
#' @return Integer: the next available revision number (max existing revision + 1)
#'
#' @details
#' This function queries all revisions of an object (across all stages) to find
#' the maximum revision number, then increments it. Throws an error if the object
#' does not exist in the database.
#'
#' @note
#' Will throw an error if object_id not found. This is part of the revision
#' tracking system where each update creates a new revision rather than modifying
#' in place.
#'
#' @keywords internal
.next_revision <- function(connection, object_type, object_id) {
  object_table <- glue::glue("{object_type}s")
  object_id_name <- glue::glue("{object_type}_id")
  revisions <- dplyr::tbl(connection, object_table) |>
    dplyr::filter(!!rlang::sym(object_id_name) == object_id) |>
    dplyr::pull("revision")

  if (length(revisions) == 0) {
    stop(glue::glue("Object {object_id} not found in {object_table}"))
  }

  max(revisions) + 1
}

#' Create new revision of an object with updated fields
#'
#' Takes an existing object and specified field updates, creates a new revision
#' with the changes. Does NOT insert to database - only prepares the revised object.
#'
#' @param connection Database connection or pool
#' @param this_object The current object (must have object_type and object_id fields)
#' @param ... Named parameters with field updates (e.g., title = "New Title")
#'
#' @return List containing the revised object with:
#'   - Specified fields updated to new values
#'   - revision incremented to next available number
#'   - All other fields unchanged from original
#'
#' @details
#' Only updates fields that exist in the table schema and are provided in `...`.
#' The object_id field cannot be updated. Uses `.update_it()` helper to safely
#' replace values (preserving existing if new is NULL). The new revision number
#' is calculated via `.next_revision()`.
#'
#' @note
#' This function only prepares the revised object - it does NOT insert it to the
#' database or destage the old revision. Use `.update_object()` for the complete
#' update workflow with transaction safety.
#'
#' @keywords internal
.revise_object <- function(connection, this_object, ...) {
  object_type <- this_object$object_type
  if (!aidr::this_exists(object_type)) {
    stop("No `object_type` to be found - is this object properly formed?")
  }
  object_id_name <- glue::glue("{object_type}_id")
  if (!aidr::this_exists(this_object[[object_id_name]])) {
    msg <- glue::glue("No `{object_id_name}` - does this object exist in the database?")
    stop(msg)
  }

  object_table_name <- glue::glue("{object_type}s")
  object_table_columns <- DBI::dbListFields(connection, object_table_name)

  submitted_updates <- list(...)
  updateable_names <- base::intersect(setdiff(object_table_columns, object_id_name), names(submitted_updates))

  updated_object <- this_object
  for (this_var in updateable_names) {
    updated_object[[this_var]] <- .update_it(this_object, this_var, submitted_updates[[this_var]])
  }

  updated_object$revision <- .next_revision(connection, object_type, this_object[[object_id_name]])

  updated_object
}

#' Update an object by creating and inserting a new revision
#'
#' Complete update workflow: revise object, destage old revision, insert new revision.
#' All operations wrapped in a database transaction for atomicity.
#'
#' @param connection Database connection (not pool - must support transactions)
#' @param this_object The current object to update (must exist in database)
#' @param ... Named parameters with field updates (e.g., title = "New Title")
#'
#' @return UUID of the updated object (same as input object_id)
#'
#' @details
#' This function implements the complete revision tracking workflow:
#' 1. Create revised object with `.revise_object()` (increments revision)
#' 2. Start database transaction
#' 3. **CRITICAL:** Destage old revision FIRST (stage = -1) via `.destage_one()`
#' 4. Insert new revision (stage = 0) via `.insert_one()`
#' 5. Commit transaction (or rollback on error)
#'
#' The transaction ensures atomicity - either both operations succeed or neither does.
#'
#' @note
#' **CRITICAL ORDERING:** `.destage_one()` MUST be called BEFORE `.insert_one()`
#' to prevent a race condition where two revisions temporarily have stage=0
#' (active) simultaneously. This could lead to duplicate active revisions if the
#' transaction is interrupted between operations. The ordering ensures at most
#' one active revision exists at any point in time.
#'
#' @keywords internal
.update_object <- function(connection, this_object, ...) {
  revised_object <- .revise_object(connection, this_object, ...)
  DBI::dbBegin(connection)
  object_insertions_outcome <- try({
    unstaging_outcome <- .destage_one(connection, this_object)
    if (!unstaging_outcome) { stop("destaging error") }
    revised_object_id <- .insert_one(connection, revised_object)
  }, silent=TRUE)
  if (inherits(object_insertions_outcome, "try-error")) {
    DBI::dbRollback(connection)
    error_msg <- attr(object_insertions_outcome, "condition")$message
    stop(glue::glue("Failed to update object: {error_msg}"))
  }
  DBI::dbCommit(connection)
  revised_object_id
}

#' Insert a validated object into the database
#'
#' Inserts a complete, validated object into its corresponding database table.
#' Expects object to have been created by `.new_object()` or `.revise_object()`.
#'
#' @param connection Database connection or pool
#' @param object List containing complete object (must have object_type field)
#'
#' @return UUID of the inserted object
#'
#' @details
#' The function:
#' 1. Extracts object_type to determine target table
#' 2. Filters out NA values (only inserts non-empty fields)
#' 3. Removes metadata object_type field (not stored in tables)
#' 4. Constructs SQL INSERT statement with glue_sql escaping
#' 5. Executes insertion and returns object UUID
#'
#' Uses `glue::glue_sql()` for SQL injection protection - all values are properly
#' escaped based on database connection type.
#'
#' @note
#' Object must be pre-validated and complete. Does not run validation or
#' augmentation - use `.insert_new_object()` if you need those steps.
#'
#' @keywords internal
.insert_one <- function(connection, object) {
  object_type <- object$object_type
  table_name <- glue::glue("{object_type}s")
  object_id <- glue::glue("{object_type}_id")
  non_empty <- object[!is.na(object)]
  non_empty$object_type <- NULL
  insert_names <- paste(names(non_empty), collapse = ", ")
  escaped_values <- glue::glue_sql("{non_empty*}", .con = connection)
  insert_statement <- glue::glue("INSERT INTO {`table_name`} ({insert_names}) VALUES ({escaped_values})")
  nrows_affected <- DBI::dbExecute(connection, insert_statement)
  object[[object_id]]
}

#' Create and insert a new object in one operation
#'
#' Convenience wrapper that combines `.new_object()` and `.insert_one()` into a
#' single operation for common object creation workflow.
#'
#' @param connection Database connection or pool
#' @param object_type Type of object (singular form: "item", "person", etc.)
#' @param object List containing object data (partial or complete)
#' @param validate_function Optional validation function to check object integrity
#' @param augment_function Optional augmentation function to enrich object data
#' @param stage Activation stage (0 = active, -1 = inactive, default: 0)
#'
#' @return UUID of the inserted object
#'
#' @details
#' Equivalent to calling:
#' ```r
#' new_obj <- .new_object(connection, object_type, object, validate_function, augment_function, stage)
#' .insert_one(connection, new_obj)
#' ```
#'
#' This is the standard way to create and insert simple objects. For items with
#' creators (which require personlists and item_persons), use the full
#' `ErudicionDB$insert_new_object()` method instead.
#'
#' @keywords internal
.insert_new_object <- function(connection, object_type, object,
                               validate_function=NULL,
                               augment_function=NULL,
                               stage=0) {
  new_object <- .new_object(connection, object_type=object_type, object=object,
                            validate_function = validate_function,
                            augment_function = augment_function,
                            stage=stage)
  .insert_one(connection, new_object)
}

#' Deactivate an object revision by setting stage to -1
#'
#' Marks a specific revision of an object as inactive (stage = -1). Used as part
#' of the update workflow to preserve audit trail while activating a new revision.
#'
#' @param connection Database connection or pool
#' @param object The object to deactivate (must have object_type, object_id, and revision)
#'
#' @return Logical: TRUE if exactly one row was updated, FALSE otherwise
#'
#' @details
#' Updates the stage field from 0 (active) to -1 (inactive) for the specified
#' object revision. The WHERE clause matches on both object_id AND revision to
#' ensure only the intended revision is affected.
#'
#' Old revisions are kept with stage=-1 for audit trail purposes rather than being
#' deleted. This preserves the complete history of all changes to an object.
#'
#' @note
#' Part of the revision tracking system. Typically called by `.update_object()`
#' as part of a transaction. Returns FALSE if no rows matched (object not found
#' or already destaged).
#'
#' @keywords internal
.destage_one <- function(connection, object) {
  # Validate required fields
  if (!("object_type" %in% names(object)) || is.null(object$object_type)) {
    stop("Object missing required 'object_type' field")
  }
  if (!("revision" %in% names(object)) || is.null(object$revision)) {
    stop("Object missing required 'revision' field")
  }

  object_type <- object$object_type
  table_name <- glue::glue("{object_type}s")
  object_id_name <- glue::glue("{object_type}_id")
  object_id <- object[[object_id_name]]

  if (is.null(object_id)) {
    stop(glue::glue("Object missing required '{object_id_name}' field"))
  }

  update_statement <- glue::glue_sql("UPDATE {`table_name`} SET stage = -1 WHERE {`object_id_name`} = {object_id} AND revision = {object$revision}",
                                     .con = connection)
  nrows_affected <- DBI::dbExecute(connection, update_statement)
  return(nrows_affected == 1)
}


#' Generate revision filter function for dplyr pipelines
#'
#' Creates a filter function that can be applied to select specific revisions
#' from query results. Used after stage filtering in retrieval operations.
#'
#' @param rev Revision selector:
#'   - "max" (default): Return only the most recent revision
#'   - "all": Return all revisions (no filtering)
#'   - <number>: Return specific revision number
#'
#' @return Function that takes a data frame (.data) and returns filtered data frame
#'
#' @details
#' This function returns a closure that can be applied in dplyr pipelines:
#' - "max" returns a function that uses `slice_max()` on revision column
#' - "all" returns a function that returns all rows (filter(TRUE))
#' - Numeric value returns a function that filters to that specific revision
#'
#' The filter is applied AFTER the stage filter in retrieval queries, meaning
#' it operates on the subset of rows already filtered by stage.
#'
#' @note
#' Used internally by `.retrieve()` and `.find()` to handle the revision parameter.
#' The returned function is called immediately in a pipeline via `()`.
#'
#' @keywords internal
.make_revision_filter <- function(rev) {
  this_filter <- function(.data) { dplyr::filter(.data, .data$revision == rev) }
  if (rev == "max") {
    this_filter <- function(.data) { dplyr::slice_max(.data, .data$revision, n=1) }
  } else if (rev == "all") {
    this_filter <- function(.data) { dplyr::filter(.data, TRUE) }
  }
  this_filter
}

#' Retrieve objects from database by ID or other column
#'
#' Retrieves one or more objects from the database by their ID (or alternative
#' column), with filtering by stage and revision.
#'
#' @param connection Database connection or pool
#' @param object_type Type of object (singular form: "item", "person", etc.)
#' @param object_id Value(s) to search for (can be vector for multiple objects)
#' @param stage Activation stage filter (0 = active, -1 = inactive, default: 0)
#' @param revision Revision filter ("max" = latest, "all" = all, or specific number)
#' @param by Column name to search on instead of object_id (default: NULL)
#' @param as_list Return as list format (TRUE, default) or data frame (FALSE)
#'
#' @return If as_list=TRUE: list of objects (each as a list). If as_list=FALSE: data frame
#'
#' @details
#' Query pipeline:
#' 1. Filter by specified column (object_id by default, or `by` parameter)
#' 2. Filter by stage (only active/inactive as specified)
#' 3. Filter by revision (latest, all, or specific number via `.make_revision_filter()`)
#' 4. Collect from database and add object_type field
#' 5. Convert to list format (if as_list=TRUE)
#'
#' Can retrieve multiple objects by passing a vector to object_id parameter.
#' The `by` parameter allows searching on alternative columns (e.g., by="orcid"
#' to retrieve persons by ORCID).
#'
#' @note
#' Stage filter is applied BEFORE revision filter. This means revision="max"
#' returns the latest revision AT the specified stage, not the latest overall.
#'
#' @keywords internal
.retrieve <- function(connection, object_type, object_id,
                      stage=0, revision="max",
                      by=NULL, as_list=TRUE) {

  table_name <- glue::glue("{object_type}s")

  if (is.null(by)) {
    id_name <- glue::glue("{object_type}_id")
  } else {
    id_name <- by
  }

  object <- dplyr::tbl(connection, table_name) |>
    dplyr::filter(!!rlang::sym(id_name) %in% object_id) |>
    dplyr::filter(.data$stage == !!stage) |>
    .make_revision_filter(revision)() |>
    dplyr::collect() |>
    dplyr::mutate(object_type = object_type)

  if (!as_list) { return(object) }
  apply(object, 1, as.list)
}

EMPTY_FIND_RESULT <- tibble::tibble(item_id = character(0),
                               stage = character(0),
                               revision = character(0),
                               found_by = character(0),
                               similarity = numeric(0))

#' Find items by external identifiers (DOI, PMID, PMCID)
#'
#' Searches for items matching any of the provided external identifiers using
#' case-insensitive LIKE matching.
#'
#' @param connection Database connection or pool
#' @param doi Digital Object Identifier (optional)
#' @param pmid PubMed ID (optional)
#' @param pmcid PubMed Central ID (optional)
#'
#' @return Data frame with columns: item_id, stage, revision, similarity (1), found_by ("identifier")
#'
#' @details
#' Builds a SQL query with OR'd WHERE clauses for each provided identifier:
#' - **SQLite**: Uses LIKE for case-insensitive matching
#' - **DuckDB**: Uses ILIKE for case-insensitive matching
#'
#' All provided identifiers are OR'd together - any match succeeds. Returns all
#' matching items with similarity=1 (exact match) and found_by="identifier".
#'
#' @note
#' Returns empty tibble if no identifiers provided or no matches found. This is
#' the first (highest priority) search strategy in `.find()` for items.
#'
#' @keywords internal
.find_item_by_identifier <- function(connection, doi=NULL, pmid=NULL, pmcid=NULL) {
  result_df <- EMPTY_FIND_RESULT
  identifiers <- list(doi=doi, pmid=pmid, pmcid=pmcid)
  where_clauses <- c()
  for (id in names(identifiers)) {
    if (aidr::this_exists(identifiers[[id]])) {
      if (is_sqlite_connection(connection)) {
        where_clauses <- c(where_clauses, glue::glue_sql("{`id`} LIKE {identifiers[[id]]}", .con = connection))
      } else {
        where_clauses <- c(where_clauses, glue::glue_sql("{`id`} ILIKE {identifiers[[id]]}", .con = connection))
      }
    }
  }
  if (length(where_clauses) > 0) {
    combined_subclauses <- paste(where_clauses, collapse = " OR ")
    search_query <- glue::glue("SELECT item_id, stage, revision FROM items WHERE {combined_subclauses}")
    fetched_results <- DBI::dbGetQuery(connection, search_query)
    if (nrow(fetched_results) > 0) {
      result_df <- fetched_results |>
        dplyr::mutate(similarity = 1, found_by = "identifier")
    }
  }
  result_df
}

#' Find items by citation metadata (year, volume, first page)
#'
#' Searches for items matching all three citation components: publication year,
#' journal volume, and first page number.
#'
#' @param connection Database connection or pool
#' @param year Publication year (integer)
#' @param volume Journal volume (string)
#' @param first_page First page number (string)
#'
#' @return Data frame with columns: item_id, stage, revision, similarity (1), found_by ("year_volume_page")
#'
#' @details
#' All three parameters are required - returns empty result if any are missing.
#' Extracts year from the issued date field using database-specific functions:
#' - **SQLite**: Uses `strftime('%Y', issued)` for date extraction, LIKE for matching
#' - **DuckDB**: Uses `year(issued)` function, ILIKE for matching
#'
#' All three criteria must match (AND'd together) for an item to be returned.
#'
#' @note
#' This is the second search strategy in `.find()` for items (after identifier
#' search fails). Particularly useful for finding items from older publications
#' that lack DOIs.
#'
#' @keywords internal
.find_item_by_year_volume_page <- function(connection, year, volume, first_page) {
  result_df <- EMPTY_FIND_RESULT
  if (aidr::this_exists(year) && aidr::this_exists(volume) && aidr::this_exists(first_page)) {
    if (is_sqlite_connection(connection)) {
      search_query <- glue::glue_sql("
        SELECT item_id, stage, revision FROM items
        WHERE strftime('%Y', issued) = {as.character(year)} AND volume LIKE {as.character(volume)} AND page_first LIKE {as.character(first_page)}",
                                     .con = connection
      )
    } else {
      search_query <- glue::glue_sql("
      SELECT item_id, stage, revision FROM items
      WHERE year(issued) = {year} AND volume ILIKE {as.character(volume)} AND page_first ILIKE {as.character(first_page)}",
                                     .con = connection
      )

    }
    fetched_results <- DBI::dbGetQuery(connection, search_query)
    if (nrow(fetched_results) > 0) {
      result_df <- fetched_results |>
        dplyr::mutate(similarity = 1, found_by = "year_volume_page")
    }
  }
  result_df
}

#' Find items by fuzzy title matching
#'
#' Searches for items with similar titles using Jaro-Winkler similarity metric,
#' with different implementations for SQLite (R-based) vs DuckDB (SQL-based).
#'
#' @param connection Database connection or pool
#' @param title Title string to search for
#'
#' @return Data frame with columns: item_id, stage, revision, similarity, found_by
#'
#' @details
#' Uses Jaro-Winkler similarity to find fuzzy title matches:
#' - Compares lowercased search title to substring of database titles (same length)
#' - **SQLite**: Pulls all items and computes similarity in R using stringdist package
#' - **DuckDB**: Uses SQL `jaro_winkler_similarity()` function with WHERE filter
#' - Threshold: similarity ≥ 0.9 (only for DuckDB; SQLite returns all, caller filters)
#'
#' The substring comparison ensures we're comparing equal-length strings, which
#' is important for Jaro-Winkler accuracy.
#'
#' @note
#' SQLite version pulls entire items table into memory for similarity calculation,
#' which may be slow for large databases. DuckDB version is more efficient as it
#' filters in the database. Both return found_by = "title".
#'
#' @keywords internal
.find_item_by_title <- function(connection, title) {
  result_df <- EMPTY_FIND_RESULT

  if (aidr::this_exists(title) && nchar(trimws(title)) > 0) {
    search_title_nchar <- nchar(title)
    if (is_sqlite_connection(connection)) {
      search_title = title

      # hack to stop package check from complaining
      item_id <- stage <- revision <- NULL

      result_df <- dplyr::tbl(connection, "items") |>
        dplyr::select(item_id, stage, revision, title) |>
        dplyr::collect() |>
        dplyr::mutate(similarity = 1-stringdist::stringdist(tolower(search_title), substring(tolower(title), 1, search_title_nchar), method="jw")) |>
        dplyr::select(-title)

      if (nrow(result_df) > 0) {
        result_df <- result_df |>
          dplyr::mutate(found_by = "title")
      }
    } else {
      search_query <- glue::glue_sql("
    SELECT
      item_id, stage, revision, jaro_winkler_similarity(lower({title}), substring(lower(title),1,{search_title_nchar})) as similarity
    FROM items WHERE similarity >= 0.9", .con = connection)
      result_df <- DBI::dbGetQuery(connection, search_query)

      if (nrow(result_df) > 0) {
        result_df <- result_df |>
          dplyr::mutate(found_by = "title")
      }
    }
  }
  result_df
}

#' Find items associated with a focal person
#'
#' Searches for all items that have a specific focal person as a creator/contributor,
#' by joining through personlists and item_persons tables.
#'
#' @param connection Database connection or pool
#' @param person_id UUID of the focal person to search for
#'
#' @return Data frame with columns: item_id, stage, revision, similarity (1), found_by ("person_id")
#'
#' @details
#' Performs a three-way join to find items linked to a person:
#' 1. items LEFT JOIN personlists (one item can have multiple personlists)
#' 2. LEFT JOIN item_persons (each personlist entry links to a person)
#' 3. Filter where person_id matches the search value
#' 4. Return unique item_ids with full item data
#'
#' This allows finding items where a focal person is listed as an author, editor,
#' or any other creator role.
#'
#' @note
#' This is the fourth search strategy in `.find()` for items. Only searches for
#' focal persons (those in the persons table), not all item_persons.
#'
#' @keywords internal
.find_item_by_person <- function(connection, person_id) {
  result_df <- fetched_results <- EMPTY_FIND_RESULT
  this_person_id <- person_id
  item_id <- NULL # hack to silence `check()`

  if (aidr::this_exists(person_id)) {
    items_tbl <- dplyr::tbl(connection, "items")
    personlists_tbl <- dplyr::tbl(connection, "personlists")
    item_persons_tbl <- dplyr::tbl(connection, "item_persons")
    found_item_ids <- items_tbl |>
      dplyr::left_join(personlists_tbl, by="item_id", suffix = c("", ".personlist"))  |>
      dplyr::left_join(item_persons_tbl, by="personlist_id", suffix = c("", ".item_person"))  |>
      dplyr::filter(person_id %in% this_person_id) |>
      dplyr::pull(item_id) |>
      unique()
    found_items <- items_tbl |>
      dplyr::filter(item_id %in% found_item_ids) |>
      dplyr::collect()
    if (nrow(found_items) > 0) {
      result_df <- found_items |>
        dplyr::mutate(similarity = 1, found_by = "person_id")
    }
  }
  result_df
}

#' Find person identifiers using flexible search strategies
#'
#' Searches person_identifiers table using one of three strategies based on which
#' fields are provided in the search object.
#'
#' @param connection Database connection or pool
#' @param object_data List containing search criteria (id_value, id_type, and/or person_id)
#'
#' @return Data frame with person_identifier records plus similarity (1) and found_by columns
#'
#' @details
#' Uses three prioritized search strategies (stops at first match):
#'
#' 1. **By id_value**: If id_value provided, match on uppercased id_value_uppercase
#'    (found_by = "identifier"). Most specific search.
#'
#' 2. **By id_type + person_id**: If both provided, match on exact combination
#'    (found_by = "person_and_type"). Useful for checking if a specific person
#'    has a specific type of identifier.
#'
#' 3. **By person_id only**: If only person_id provided, return all identifiers
#'    for that person (found_by = "person_id").
#'
#' All matches return similarity=1 (exact match).
#'
#' @note
#' The id_value_uppercase field stores normalized (uppercased) identifier values
#' for case-insensitive matching. Used by `.find()` when searching for person_identifier
#' objects.
#'
#' @keywords internal
.find_person_identifier <- function(connection, object_data) {
  found <- tibble::tibble()
  id_value_uppercase <- id_type <- person_id <- NULL # to silence `check()`
  if (aidr::this_exists(object_data$id_value)) {
    result_df <- dplyr::tbl(connection, "person_identifiers") |>
      dplyr::filter(id_value_uppercase == toupper(object_data$id_value)) |>
      dplyr::collect()
    if (nrow(result_df) > 0) {
      found <- result_df |>
        dplyr::mutate(similarity = 1, found_by = "identifier")
    }
  } else if (aidr::this_exists(object_data$id_type) && aidr::this_exists(object_data$person_id)) {
    result_df <- dplyr::tbl(connection, "person_identifiers") |>
      dplyr::filter(id_type == object_data$id_type, person_id == object_data$person_id) |>
      dplyr::collect()
    if (nrow(result_df) > 0) {
      found <- result_df |>
        dplyr::mutate(similarity = 1, found_by = "person_and_type")
    }
  } else if (aidr::this_exists(object_data$person_id)) {
    result_df <- dplyr::tbl(connection, "person_identifiers") |>
      dplyr::filter(person_id == object_data$person_id) |>
      dplyr::collect()
    if (nrow(result_df) > 0) {
      found <- result_df |>
        dplyr::mutate(similarity = 1, found_by = "person_id")
    }
  }
  found
}

#' Find objects using hierarchical search strategies
#'
#' Attempts to find matching objects in the database using multiple search strategies
#' in priority order, with specialized logic for items and person_identifiers.
#'
#' @param connection Database connection or pool
#' @param object_type Type of object (singular form: "item", "person_identifier", etc.)
#' @param object_data List containing search criteria (fields to match)
#' @param stage Activation stage filter (0 = active, -1 = inactive, default: 0)
#' @param revision Revision filter ("max" = latest, "all" = all, or specific number)
#'
#' @return Data frame with matching objects plus similarity and found_by columns
#'
#' @details
#' **For items**, uses hierarchical search (stops at first match):
#' 1. By identifier (DOI, PMID, PMCID) via `.find_item_by_identifier()`
#' 2. By year/volume/first_page via `.find_item_by_year_volume_page()`
#' 3. By title (Jaro-Winkler similarity ≥ 0.9) via `.find_item_by_title()`
#' 4. By associated person_id via `.find_item_by_person()`
#' 5. Fallback: Inner join on all provided fields
#'
#' **For person_identifiers**, uses `.find_person_identifier()` with three strategies.
#'
#' **For other object types**, uses direct inner join on all provided fields.
#'
#' Results include:
#' - `similarity`: Numeric similarity score (1.0 for exact matches, <1.0 for fuzzy)
#' - `found_by`: String indicating which search strategy succeeded
#'
#' @note
#' Stage and revision filters applied AFTER search strategies run. Returns empty
#' tibble if no matches found. Each search function defines what constitutes a match.
#'
#' @keywords internal
.find <- function(connection, object_type, object_data,
                  stage = 0, revision = "max") {

  # default to having found nothing
  found <- tibble::tibble()

  # then, actually try to find it
  # - first, by identifiers that are part of the citeproc item schema
  if (object_type == "item") {
    found <- .find_item_by_identifier(
      connection = connection,
      doi = object_data$doi,
      pmid = object_data$pmid,
      pmcid = object_data$pmcid
    )
    if (nrow(found) == 0 && aidr::this_exists(object_data$issued)) {
      found <- .find_item_by_year_volume_page(
        connection = connection,
        year = lubridate::year(object_data$issued),
        volume = object_data$volume,
        first_page = object_data$page_first
      )
    }
    if (nrow(found) == 0) {
      found <- .find_item_by_title(
        connection = connection,
        title = object_data$title
      )
    }
    if (nrow(found) == 0) {
      found <- .find_item_by_person(
        connection = connection,
        person_id = object_data$person_id
      )
    }
  } else if (object_type == "person_identifier") {
    found <- .find_person_identifier(connection, object_data)
  }

  if (nrow(found) == 0) {
    con <- pool::poolCheckout(connection)

    tryCatch({
      search_table <- dplyr::tbl(con, glue::glue("{object_type}s"))
      found <- search_table |>
        # this join is what generates the "by = join_by(whatever)" messages
        dplyr::inner_join(tibble::as_tibble(object_data), copy=TRUE) |>
        dplyr::collect()
    }, finally = {
      pool::poolReturn(con)
    })
  }

  found <- found |>
    dplyr::filter(stage == stage) |>
    .make_revision_filter(revision)()

  found
}

#' Check if a citation key already exists in the database
#'
#' Queries the items table to determine if a citation key is already in use,
#' used during citation key generation to ensure uniqueness.
#'
#' @param connection Database connection or pool
#' @param citekey Citation key string to check
#'
#' @return Logical: TRUE if citekey exists, FALSE otherwise
#'
#' @details
#' Performs a simple SELECT query for the citation_key. Returns TRUE if any rows
#' are found, FALSE if none. Used by the citation key generation algorithm in
#' `insert_new_item()` to avoid collisions.
#'
#' @note
#' Part of the citation key uniqueness workflow. If collision detected, the
#' generation algorithm adds numeric suffixes (e.g., Smith2020Example1) until
#' a unique key is found.
#'
#' @keywords internal
.citekey_exists_in_db <- function(connection, citekey) {
  count_query <- glue::glue_sql("SELECT citation_key FROM items WHERE citation_key = {citekey}", .con = connection)
  count_result <- DBI::dbGetQuery(connection, count_query)
  this_count <- nrow(count_result)
  assertthat::assert_that(length(this_count) == 1, msg="Found something other than a single count value")
  this_count > 0
}

#' Match item_person data to focal persons using hierarchical strategies
#'
#' Attempts to match a person from imported bibliography data (item_person) to an
#' existing focal person in the persons table using four prioritized matching strategies.
#'
#' @param connection Database connection or pool
#' @param pdata List with person data containing: family, given, orcid (optional), literal (optional)
#' @param only_most_recent Use only latest revision of persons (default: TRUE)
#' @param only_active_stage Use only active persons (stage=0) (default: TRUE)
#'
#' @return Data frame with matched person(s) plus similarity and found_by columns,
#'   or empty data frame if no match
#'
#' @details
#' **Hierarchical matching algorithm** (stops at first match):
#'
#' 1. **ORCID exact match**: If pdata has orcid, match on orcid field (found_by = "orcid")
#' 2. **Full name exact match**: Match primary_given_names + surnames, or ASCII variants
#'    (found_by = "full_name_match")
#' 3. **Initials + surname match**: Match initials extracted from given name plus surname,
#'    checking both primary and ASCII variants (found_by = "initials_match")
#' 4. **Approximate name match**: Fuzzy match using Jaro-Winkler distance on first word
#'    of given name + surname, threshold < 0.05 distance (found_by = "approximate_name_match")
#'
#' All exact matches return similarity=1.0. Approximate matches return
#' similarity = 1 - mean(given_distance, family_distance).
#'
#' @note
#' - Only matches actual people - institutional authors (literal without family/given) return NULL
#' - Requires at least family OR given name to attempt matching
#' - Pulls full persons table into memory (assumes small size for intended use cases)
#' - ASCII transliteration handles accented characters (e.g., José → jose)
#' - If multiple persons match, all are returned (caller handles ambiguity)
#'
#' @keywords internal
.match_person <- function(connection, pdata, only_most_recent = TRUE, only_active_stage = TRUE) {
  # stop check warnings
  person_id <- revision <- stage <- orcid <- primary_given_names <- NULL
  surnames <- ascii_given_names <- ascii_surnames <- initials <- NULL
  family_distance <- given_distance <- NULL

  # need to have at least a `family` or `given` name
  pdata_names <- names(pdata)
  if (!("given" %in% pdata_names || "family" %in% pdata_names)) { return(NULL) }


  # pdata <- pdata |>
  #   mutate(given_ascii = stringi::stri_trans_general(given, id = "Latin-ASCII"),
  #          family_ascii = stringi::stri_trans_general(family, id = "Latin-ASCII"))

  pdata$given_ascii <- if (!is.null(pdata$given) && !is.na(pdata$given)) {
    tolower(stringi::stri_trans_general(pdata$given, id = "Latin-ASCII"))
  } else {
    NA_character_
  }

  pdata$family_ascii <- if (!is.null(pdata$family) && !is.na(pdata$family)) {
    tolower(stringi::stri_trans_general(pdata$family, id = "Latin-ASCII"))
  } else {
    NA_character_
  }

  # Instead of sending a bunch of queries to the database,
  #  just pull the full `persons` table here.
  # This assumes that the `persons` table is small,
  #  which is true for the core intended use cases
  persons <- dplyr::tbl(connection, "persons") |>
    dplyr::collect()

  if (only_most_recent) {
    persons <- persons |>
      dplyr::group_by(person_id) |>
      dplyr::arrange(dplyr::desc(revision)) |>
      dplyr::slice(1) |>
      dplyr::ungroup()
  }

  if (only_active_stage) {
    persons <- persons |> dplyr::filter(stage == 0)
  }

  # first, try to find an orcid match
  if ("orcid" %in% pdata_names) {
    this_person <- persons |> dplyr::filter(orcid == pdata$orcid)
    if (nrow(this_person) >= 1) {
      return(
        this_person |>
          dplyr::mutate(similarity = 1, found_by = "orcid")
      )
    }
  }

  # second, look for a perfect first and last name match
  if ("given" %in% pdata_names && "family" %in% pdata_names) {
    this_person <- persons |> dplyr::filter(primary_given_names == pdata$given,
                                     surnames == pdata$family)
    if (nrow(this_person) == 0) {
      this_person <- persons |> dplyr::filter(ascii_given_names == pdata$given_ascii,
                                              ascii_surnames == pdata$family_ascii)
    }
    if (nrow(this_person) >= 1) {
      return(
        this_person |>
          dplyr::mutate(similarity = 1, found_by = "full_name_match")
      )
    }
  }

  # add initials column
  persons <- persons |>
    dplyr::mutate(initials = .initials(ascii_given_names))

  # third, look for a perfect initials and last name match
  if ("given" %in% pdata_names && "family" %in% pdata_names) {
    this_person <- persons |> dplyr::filter(tolower(initials) == tolower(.initials(pdata$given)), surnames == pdata$family)
    if (nrow(this_person) == 0) {
      this_person <- persons |> dplyr::filter(tolower(initials) == tolower(.initials(pdata$given)), ascii_surnames == pdata$family_ascii)
    }
    if (nrow(this_person) >= 1) {
      return(
        this_person |>
          dplyr::mutate(similarity = 1, found_by = "initials_match")
      )
    }
  }

  # fourth, look for a close first and last name match
  if ("given" %in% pdata_names && "family" %in% pdata_names) {
    # use only first word in ascii given name (initials are not in the primary_given_names column)
    first_given <- stringr::str_extract(pdata$given_ascii, "\\w+")

    # Skip this matching strategy if no word characters in given name
    if (!is.na(first_given)) {
      this_person <- persons |>
        dplyr::mutate(given_distance = .name_distance(first_given, ascii_given_names),
               family_distance = .name_distance(pdata$family_ascii, ascii_surnames)) |>
        dplyr::filter(given_distance < 0.05, family_distance < 0.05)
      if (nrow(this_person) >= 1) {
        return(
          this_person |>
            dplyr::mutate(similarity = 1-mean(c(given_distance, family_distance)), found_by = "approximate_name_match")
        )
      }
    }
  }

  # Return empty result with correct column structure (persons table + similarity + found_by)
  return(
    persons |>
      dplyr::filter(FALSE) |>
      dplyr::mutate(similarity = numeric(0), found_by = character(0))
  )
}


#' Get bibliography items for a focal person
#'
#' Finds all items associated with a focal person (via item_persons → personlists)
#' and enriches them with creator data for bibliography generation.
#'
#' @param connection Database connection or pool
#' @param person_id UUID of the focal person
#'
#' @return Data frame with items and their associated creators (via `.item_ids_to_biblio_items()`)
#'
#' @details
#' This is the first step in the person bibliography workflow:
#' 1. Find item_persons entries linking to this person_id (stage=0 only)
#' 2. Join to personlists to get associated item_ids
#' 3. Get distinct item_ids
#' 4. Pass to `.item_ids_to_biblio_items()` for enrichment
#'
#' Only returns active (stage=0) associations. The resulting data frame includes
#' full item data plus list-columns for each personlist_type (author, editor, etc.).
#'
#' @note
#' Used as entry point for generating a focal person's bibliography. Chains to
#' `.item_ids_to_biblio_items()` → `.items_to_biblio_items()` for full enrichment.
#'
#' @keywords internal
.person_id_to_biblio_items <- function(connection, person_id) {
  result_df <- fetched_results <- tibble::tibble()
  # following to avoid check() notes
  stage <- personlist_id <- NULL

  this_person_id <- person_id
  item_id <- NULL # hack to silence `check()`

  if (aidr::this_exists(this_person_id)) {
    item_persons_for_this_person_id <- dplyr::tbl(connection, "item_persons") |>
      dplyr::filter(stage == 0, !is.na(person_id)) |>
      dplyr::select(personlist_id, person_id) |>
      dplyr::filter(person_id == this_person_id)

    item_ids <- dplyr::tbl(connection, "personlists") |>
      dplyr::filter(stage == 0) |>
      dplyr::select(personlist_id, item_id) |>
      dplyr::distinct() |>
      dplyr::right_join(item_persons_for_this_person_id, by="personlist_id") |>
      dplyr::select(item_id) |>
      dplyr::distinct() |>
      dplyr::collect()

    result_df <- .item_ids_to_biblio_items(connection, item_ids)

  }
  result_df
}


#' Convert item IDs to enriched bibliography items
#'
#' Takes item IDs (as data frame, character vector, or UUIDs), retrieves items
#' from database, and enriches with creator data for bibliography generation.
#'
#' @param connection Database connection or pool
#' @param item_ids Item IDs in one of three formats:
#'   - Data frame with "item_id" column
#'   - Character vector of UUIDs
#'   - UUID vector (converted to character)
#'
#' @return Data frame with items and creator list-columns (via `.items_to_biblio_items()`)
#'
#' @details
#' Workflow:
#' 1. Normalize item_ids to data frame format
#' 2. Check out connection from pool (for copy=TRUE join safety)
#' 3. Right join items table to item_ids (active stage=0 only)
#' 4. Filter out non-existent items (NA revision from right join)
#' 5. Pass to `.items_to_biblio_items()` for creator enrichment
#' 6. Return connection to pool
#'
#' UUIDs are converted to character for database compatibility (stored as VARCHAR).
#'
#' @note
#' Middle step in bibliography pipeline. Chains from `.person_id_to_biblio_items()`
#' and chains to `.items_to_biblio_items()`. Uses poolCheckout/poolReturn for
#' transaction safety.
#'
#' @keywords internal
.item_ids_to_biblio_items <- function(connection, item_ids) {
  result_df <- fetched_results <- tibble::tibble()
  # following to avoid check() notes
  stage <- revision <- NULL

  item_id_in_df <- NULL
  if (is.data.frame(item_ids) && "item_id" %in% names(item_ids)) {
    item_id_in_df <- item_ids[,"item_id"]
  } else if (is.character(item_ids) || uuid::is.UUID(item_ids)) {
    # Keep as character for database compatibility with copy=TRUE
    # Database stores UUIDs as VARCHAR anyway
    item_id_in_df <- tibble::tibble(item_id = as.character(item_ids))
  } else {
    return(result_df)
  }

  checked_out_con <- pool::poolCheckout(connection)
  tryCatch({
    items_tbl <- dplyr::tbl(checked_out_con, "items") |> dplyr::filter(stage == 0)
    all_items <- items_tbl |>
      dplyr::right_join(item_id_in_df, by="item_id", copy=TRUE) |>
      dplyr::collect() |>
      # Filter out non-existent items (right_join creates rows with NA revision)
      dplyr::filter(!is.na(revision))
  }, finally = {
    pool::poolReturn(checked_out_con)
  })

  result_df <- .items_to_biblio_items(connection, all_items)

  result_df
}



#' Enrich items with creator data for bibliography generation
#'
#' Takes a data frame of items and joins in all associated creator data from
#' personlists and item_persons, organized by personlist_type (author, editor, etc.).
#'
#' @param connection Database connection or pool
#' @param items Data frame of item records (from database)
#'
#' @return Data frame with original items plus list-columns for each personlist_type
#'
#' @details
#' Complex enrichment workflow:
#' 1. Extract just item_ids from items data frame
#' 2. Join personlists to get personlist metadata (type, id)
#' 3. Group personlists by personlist_type (author, editor, etc.)
#' 4. For each type, join item_persons to get creator details
#' 5. Group item_persons by item_id within each type
#' 6. Create list-columns containing creator data frames for each item+type
#' 7. Full join all type-specific data back together
#' 8. Left join to original items to preserve all item data
#'
#' Result has structure: items table columns + list-columns named by personlist_type
#' (e.g., `author` column contains data frame of authors for each item).
#'
#' @note
#' Uses multiple poolCheckout/poolReturn cycles for each personlist_type join.
#' Handles empty personlists gracefully (items without creators). This is the
#' most complex function in the bibliography pipeline.
#'
#' @keywords internal
.items_to_biblio_items <- function(connection, items) {
  result_df <- fetched_results <- tibble::tibble()
  # following to avoid check notes
  stage <- personlist_id <- item_id <- personlist_type <- NULL

  if (aidr::this_exists(items)) {

    just_item_ids <- items[,"item_id"]

    checked_out_con <- pool::poolCheckout(connection)
    tryCatch({
      personlists_tbl <- dplyr::tbl(checked_out_con, "personlists") |> dplyr::filter(stage == 0)
      personlist_data <- personlists_tbl |>
        dplyr::right_join(just_item_ids, by="item_id", copy=TRUE) |>
        dplyr::select(personlist_id, item_id, personlist_type) |>
        dplyr::collect() |>
        # Filter out rows with NA personlist_id (items with no personlists)
        dplyr::filter(!is.na(personlist_id))
    }, finally = {
      pool::poolReturn(checked_out_con)
    })
    # Check for empty personlist data early
    if (nrow(personlist_data) == 0) {
      # No personlists for these items - skip processing
      result_df <- items
      # No need to add person columns
    } else {
      personlists_grouped_by_type <- personlist_data |>
        dplyr::group_by(personlist_type)

      personlists_by_type <- personlists_grouped_by_type |> dplyr::group_split(.keep=FALSE)
      names(personlists_by_type) <- (personlists_grouped_by_type |> dplyr::group_keys())$personlist_type

      item_persons_by_type <- vector("list", length(personlists_by_type))
      names(item_persons_by_type) <- names(personlists_by_type)

      for (ptype in names(personlists_by_type)) {
        checked_out_con <- pool::poolCheckout(connection)
        tryCatch({
          this_item_persons_tbl <- dplyr::tbl(checked_out_con, "item_persons") |> dplyr::filter(stage == 0)
          these_item_persons <- this_item_persons_tbl |>
            dplyr::right_join(personlists_by_type[[ptype]], by="personlist_id", copy=TRUE) |>
            dplyr::collect() |>
            dplyr::group_by(item_id)
          item_persons_by_type[[ptype]] <- tibble::tibble(
            item_id = (these_item_persons |> dplyr::group_keys())$item_id,
            {{ptype}} := these_item_persons |> dplyr::group_split()
          )
        }, finally = {
          pool::poolReturn(checked_out_con)
        })
      }

      combined_item_persons_by_personlist_type <- purrr::reduce(item_persons_by_type, dplyr::full_join, by = 'item_id')

      result_df <- items |>
        dplyr::left_join(combined_item_persons_by_personlist_type, by="item_id")

    }
  }
  result_df
}

#' Convert bibliography items to CSL-compliant list structure
#'
#' Transforms enriched bibliography items data frame into CSL (Citation Style Language)
#' format for export to JSON bibliography files.
#'
#' @param bibitems Data frame from `.items_to_biblio_items()` with item data and creator list-columns
#'
#' @return Named list of CSL-formatted items (names are item_ids)
#'
#' @details
#' Transformation pipeline:
#' 1. Convert date columns to character (avoid JSON epoch seconds)
#' 2. Convert rows to list format
#' 3. Filter out NA values via `.lapply_filter_na()`
#' 4. Filter out internal metadata via `.lapply_filter_internal()`
#' 5. Rename fields to CSL hyphenated format:
#'    - citation_key → id
#'    - container_title → container-title
#'    - page_first → page-first
#'    - (and 20+ other snake_case → hyphenated conversions)
#' 6. Transform creator data via `.item_persons_to_biblio_persons()`
#'
#' Result is ready for `.csl_list_to_json()` export.
#'
#' @note
#' Handles empty input gracefully (returns empty list). Date conversion prevents
#' pandoc bibliography processing errors. The id field (citation_key) is used
#' as the cite key in LaTeX/Markdown.
#'
#' @keywords internal
.biblio_items_to_csl_list <- function(bibitems) {
  # Handle empty input early
  if (nrow(bibitems) == 0) { return(list()) }

  # Convert date columns to character, only if they exist
  # - because toJSON otherwise converts dates to seconds since 1970,
  #    which pandoc bibliography processing does not like
  date_cols_present <- intersect(item_date_columns, names(bibitems))
  if (length(date_cols_present) > 0) {
    items <- bibitems |>
      dplyr::mutate(dplyr::across(dplyr::all_of(date_cols_present), as.character))
  } else {
    items <- bibitems
  }

  items <- apply(items, 1, as.list)
  if (length(items) == 0) { return(list()) }
  names(items) <- sapply(items, '[[', "item_id")
  items <- items |>
    .lapply_filter_na() |>
    .lapply_filter_internal() |>
    purrr::map(\(x) .rename_element(x, "citation_key", "id")) |>
    purrr::map(\(x) .rename_element(x, "container_title_short", "container-title-short")) |>
    purrr::map(\(x) .rename_element(x, "container_title", "container-title")) |>
    purrr::map(\(x) .rename_element(x, "available_date", "available-date")) |>
    purrr::map(\(x) .rename_element(x, "event_date", "event-date")) |>
    purrr::map(\(x) .rename_element(x, "event_title", "event-title")) |>
    purrr::map(\(x) .rename_element(x, "event_place", "event-place")) |>
    purrr::map(\(x) .rename_element(x, "original_date", "original-date")) |>
    purrr::map(\(x) .rename_element(x, "archive_place", "archive-place")) |>
    purrr::map(\(x) .rename_element(x, "call_number", "call-number")) |>
    purrr::map(\(x) .rename_element(x, "chapter_number", "chapter-number")) |>
    purrr::map(\(x) .rename_element(x, "citation_number", "citation-number")) |>
    purrr::map(\(x) .rename_element(x, "citation_label", "citation-label")) |>
    purrr::map(\(x) .rename_element(x, "collection_number", "collection-number")) |>
    purrr::map(\(x) .rename_element(x, "collection_title", "collection-title")) |>
    purrr::map(\(x) .rename_element(x, "first_reference_note_number", "first-reference-note-number")) |>
    purrr::map(\(x) .rename_element(x, "number_of_pages", "number-of-pages")) |>
    purrr::map(\(x) .rename_element(x, "number_of_volumes", "number-of-volumes")) |>
    purrr::map(\(x) .rename_element(x, "original_publisher_place", "original-publisher-place")) |>
    purrr::map(\(x) .rename_element(x, "original_publisher", "original-publisher")) |>
    purrr::map(\(x) .rename_element(x, "original_title", "original-title")) |>
    purrr::map(\(x) .rename_element(x, "page_first", "page-first")) |>
    purrr::map(\(x) .rename_element(x, "part_title", "part-title")) |>
    purrr::map(\(x) .rename_element(x, "publisher_place", "publisher-place")) |>
    purrr::map(\(x) .rename_element(x, "reviewed_genre", "reviewed-genre")) |>
    purrr::map(\(x) .rename_element(x, "reviewed_title", "reviewed-title")) |>
    purrr::map(\(x) .rename_element(x, "title_short", "title-short")) |>
    purrr::map(\(x) .rename_element(x, "year_suffix", "year-suffix"))

  items <- items |>
    purrr::map(.item_persons_to_biblio_persons)

  items

}

#' Transform item_persons data to CSL-compliant person format
#'
#' Converts item_persons data frames (in list-columns) to CSL person list format,
#' renaming fields to hyphenated CSL names and filtering metadata.
#'
#' @param list_item Single item from CSL list (has item data + personlist type list-columns)
#'
#' @return Same list_item with personlist data transformed to CSL person format
#'
#' @details
#' For each personlist_type in the item (author, editor, etc.):
#' 1. Extract the persons data frame from list-column
#' 2. Rename optional CSL columns to hyphenated format:
#'    - dropping_particle → dropping-particle
#'    - non_dropping_particle → non-dropping-particle
#'    - comma_suffix → comma-suffix
#'    - static_ordering → static-ordering
#'    - parse_names → parse-names
#' 3. Arrange by position field
#' 4. Convert each row to a list (preserving structure)
#' 5. Filter out NAs and internal metadata (keep person_id)
#' 6. Replace list-column with CSL person list
#'
#' @note
#' Handles NULL, empty, and non-dataframe personlists gracefully. The person_id
#' field is kept (via keep="person_id") for potential focal person tracking.
#'
#' @keywords internal
.item_persons_to_biblio_persons <- function(list_item) {
  # to avoid check() notes
  position <- NULL

  plists <- base::intersect(valid_personlist_types, names(list_item))
  if (length(plists) == 0) { return(list_item) }

  for (this_type in plists) {
    # Get the person data
    persons <- list_item[[this_type]]

    # Handle NULL cases
    if (is.null(persons)) {
      next
    }

    # If persons is a list (list-column structure from as.list()),
    # extract the first element which should be the dataframe
    if (is.list(persons) && !is.data.frame(persons)) {
      if (length(persons) == 0) {
        next
      }
      persons <- persons[[1]]
    }

    # Now check if it's a dataframe
    if (!is.data.frame(persons)) {
      next
    }
    if (nrow(persons) == 0) {
      next
    }

    # Only rename optional CSL columns that exist
    if ("dropping_particle" %in% names(persons)) {
      persons <- persons |> dplyr::rename("dropping-particle" = "dropping_particle")
    }
    if ("non_dropping_particle" %in% names(persons)) {
      persons <- persons |> dplyr::rename("non-dropping-particle" = "non_dropping_particle")
    }
    if ("comma_suffix" %in% names(persons)) {
      persons <- persons |> dplyr::rename("comma-suffix" = "comma_suffix")
    }
    if ("static_ordering" %in% names(persons)) {
      persons <- persons |> dplyr::rename("static-ordering" = "static_ordering")
    }
    if ("parse_names" %in% names(persons)) {
      persons <- persons |> dplyr::rename("parse-names" = "parse_names")
    }

    # Convert each row to a list properly
    # We need to preserve the data structure without matrix coercion
    persons_arranged <- persons |> dplyr::arrange(position)
    person_list <- vector("list", nrow(persons_arranged))
    for (i in seq_len(nrow(persons_arranged))) {
      person_list[[i]] <- as.list(persons_arranged[i, ])
    }

    list_item[[this_type]] <- person_list |>
      purrr::map(.filter_na) |>
      purrr::map(\(x) .filter_internal(x, keep="person_id"))

  }

  list_item
}

#' Convert CSL list to JSON bibliography string
#'
#' Final step in bibliography pipeline: converts CSL-formatted list to JSON string
#' suitable for export to .json bibliography files (for pandoc, LaTeX, etc.).
#'
#' @param csl_list Named list from `.biblio_items_to_csl_list()`
#'
#' @return JSON string with CSL bibliography data
#'
#' @details
#' Simple wrapper around jsonlite::toJSON with appropriate settings:
#' - Removes names (convert to unnamed array)
#' - pretty=TRUE for readable output
#' - auto_unbox=TRUE to prevent single values becoming arrays
#'
#' The resulting JSON is compatible with CSL processors like pandoc-citeproc.
#'
#' @keywords internal
.csl_list_to_json <- function(csl_list) {
  names(csl_list) <- NULL
  items.json <- jsonlite::toJSON(csl_list, pretty=TRUE, auto_unbox=TRUE)
  return(items.json)
}


# this is a nothing function that has no use other than to stop the
# R package build check function complaining about unused dbplyr
# (that needs to be loaded for using dplyr with databases,
#  but isn't actually directly called)
.make_check_dbplyr_note_go_away <- function(con) {
  dbplyr::dbplyr_edition(con)
}

## DB Interface Object

#' @title ErudicionDB Object
#'
#' @description
#' Interface for an ErudicionDB.
#'
#' @export
#'
#' @include augmentors.R
#' @include validators.R
ErudicionDB <- R6::R6Class(classname = "erudicion_db_object", # inherit = R6P::Singleton,
  private = list(
    pool = NULL,
    validators = VALIDATORS,
    augmentors = AUGMENTORS,
    #' @description
    #' Insert a new item from an online repository
    #'
    #' @param item_data A list with item and creator entries
    #' @param stage Activation stage
    insert_new_item = function(item_data, stage) {
      # before we start, create a citation_key
      citekey_surname <- .select_surname(item_data)
      citekey_year <- .select_year(item_data)
      citekey_title <- .select_title(item_data)
      potential_citekey <- .make_citekey(citekey_surname, citekey_year, citekey_title)
      if (.citekey_exists_in_db(self$con, potential_citekey)) {
        potential_citekey <- .make_citekey(citekey_surname, citekey_year, citekey_title, n=2)
      }
      if (.citekey_exists_in_db(self$con, potential_citekey)) {
        potential_citekey_base <- .make_citekey(citekey_surname, citekey_year, citekey_title)
        potential_citekey <- potential_citekey_base
        i <- 0
        while(.citekey_exists_in_db(self$con, potential_citekey)) {
          i <- i + 1
          potential_citekey <- paste0(potential_citekey_base, i)
        }
      }
      item_data$item$citation_key <- potential_citekey

      this_item_id <- pool::poolWithTransaction(private$pool, function(conn) {
        # first, insert the core item into the items table
        this_item_id <- .insert_new_object(conn, "item", item_data$item,
                                           validate_function = private$validators[["item"]],
                                           augment_function = private$augmentors[["item"]],
                                           stage=stage)

        # second, find all the valid personlists in the item data
        personlists <- base::intersect(valid_personlist_types, names(item_data))

        # and then, for each personlist, add
        # 1. the list
        # 2. its people
        # 3. (if present) their affiliations
        for (plist in personlists) {
          plist_id <- .insert_new_object(conn, "personlist", list(item_id = this_item_id,
                                                                personlist_type = plist),
                                         validate_function = private$validators[["personlist"]],
                                         augment_function = private$augmentors[["personlist"]],
                                         stage=stage)

          this_plist <- item_data[[plist]]
          this_affiliation <- item_data[[glue::glue("{plist}_affiliation")]]
          for (i in seq_along(this_plist)) {
            this_person <- this_plist[[i]]
            # look to see if this item person is in one of our focal people
            found_person <- .match_person(conn, this_person)
            found_person_issue <- NULL
            if (aidr::this_exists(found_person)) {
              if (nrow(found_person) == 1) {
                this_person$person_id = found_person$person_id
              } else {
                # more than one match, but don't have item_person_id yet, so just create an issue
                #  that will be updated below after the person is entered into the database
                this_person_issue <- list(object_type="item_person", status="open", description="multiple persons matched")
              }
            }
            this_person$personlist_id <- plist_id
            this_person$position <- i
            # add to db
            this_item_person_id <- .insert_new_object(conn, "item_person", this_person,
                                                      validate_function = private$validators[["item_person"]],
                                                      augment_function = private$augmentors[["item_person"]])

            # if an issue was found earlier, add it to the database
            if (aidr::this_exists(found_person_issue)) {
              found_person_issue$object_id <- this_item_person_id
              .insert_new_object(conn, "issue", found_person_issue,
                                 validate_function = private$validators[["issue"]],
                                 augment_function = private$augmentors[["issue"]],
                                 stage=stage)
            }
            if (aidr::this_exists(this_affiliation) && aidr::this_exists(this_affiliation[[i]])) {
              for (j in seq_along(this_affiliation[[i]])) {
                this_affiliation_entry <- list(
                  item_person_id = this_item_person_id,
                  position = j,
                  affiliation = this_affiliation[[i]][j]
                )
                .insert_new_object(conn, "affiliation_reference", this_affiliation_entry,
                                   validate_function = private$validators[["affiliation_reference"]],
                                   augment_function = private$augmentors[["affiliation_reference"]],
                                   stage=stage)
              }
            }
          }
        }
        return(this_item_id)
      })
      return(this_item_id)
    },
    #' @description
    #' Clean up after removal of ErudicionDB object
    finalize = function() {
      self$disconnect()
    }
  ),
  public = list(
    #' @description
    #' Create new ErudicionDB object
    #'
    #' @param dbargs_list A list of parameters to create the database
    initialize = function(dbargs_list) {
      # super$initialize()
      self$establish_connection(dbargs_list)
    },
    #' @description
    #' Create ErudicionDB object and connect to a database
    #'
    #' @param dbargs_list A list of parameters to create the database
    establish_connection = function(dbargs_list) {
      if (is.null(private$pool) || !dbIsValid(private$pool)) {
        private$pool <- do.call(pool::"dbPool", dbargs_list)
      }
      invisible(self)
    },
    #' @description
    #' Disconnect from a database
    #'
    #' @returns Nothing
    disconnect = function() {
      if (!is.null(private$pool)) {
        pool::poolClose(private$pool)
        private$pool <- NULL
      }
    },
    #' @description
    #' Generate a data frame interface to a database table
    #'
    #' @param tbl_name A database table name
    tbl = function(tbl_name) {
      return(dplyr::tbl(private$pool, tbl_name))
    },
    #' @description
    #' Add a function to validate a new database table entry
    #'
    #' @param for_what The singular name of the table
    #' @param validate_function A function
    add_validator = function(for_what, validate_function) {
      private$validators[[for_what]] <- validate_function
    },
    #' @description
    #' Add a function to augment a new database table entry
    #'
    #' @param for_what The singular name of the table
    #' @param augment_function A function
    add_augmentor = function(for_what, augment_function) {
      private$augmentors[[for_what]] <- augment_function
    },
    #' @description
    #' Fully create, augment, and validate object for insertion into database.
    #'
    #' @param object_type Kind of object (singular of table name)
    #' @param object List of object data
    #' @param stage Activation stage (default: 0=Active)
    new_object = function(object_type, object, stage=0) {
      .new_object(self$con, object_type=object_type, object=object,
                  augment_function = private$augmentors[[object_type]],
                  validate_function = private$validators[[object_type]],
                  stage=stage)
    },
    #' @description
    #' Insert a properly created, augmented, and validated object into the db.
    #'
    #' @param object List with proper object for database.
    insert_object = function(object) {
      .insert_one(self$con, object)
    },
    #' @description
    #' Properly create and insert an object into db in one call.
    #'
    #' @param object_type Kind of object (singular of table name)
    #' @param object List of object data
    #' @param stage Activation stage (default: 0=Active)
    insert_new_object = function(object_type, object, stage=0) {
      if (object_type == "item" && "item" %in% names(object)) {
        new_object_id <- private$insert_new_item(object, stage=stage)
      } else {
        new_object_id <- pool::poolWithTransaction(self$con, function(conn) {
          new_object <- .new_object(conn, object_type=object_type, object=object,
                      augment_function = private$augmentors[[object_type]],
                      validate_function = private$validators[[object_type]],
                      stage=stage)
          .insert_one(conn, new_object)
        })
      }
      new_object_id
    },
    #' @description
    #' Retrieve objects from the database.
    #'
    #' Note that submitting a vector of object_id's will return multiple
    #' objects. Also note that the search can use a different column using
    #' the `by` parameter, in which case the `object_id` parameter is the
    #' search term(s) for that other column.
    #'
    #' @param object_type Kind of object (singular of table name)
    #' @param object_id The object_id in the database
    #' @param stage Activation stage (default: 0=Active)
    #' @param revision Revision of the object (default: "max"=latest)
    #' @param by Retrieve by some column other than object_id (default: NULL=no)
    #' @param as_list Return as list (default: TRUE; FALSE = as data frame)
    retrieve = function(object_type, object_id, stage=0, revision="max",
                          by=NULL, as_list=TRUE) {
      .retrieve(self$con, object_type=object_type, object_id=object_id,
       stage=stage, revision=revision, by=by, as_list=as_list)
    },
    #' @description
    #' Find an object in the database.
    #'
    #' @param object_type Kind of object (singular of table name)
    #' @param object_data An object in list form
    #' @param stage Activation stage (default: 0=Active)
    #' @param revision Revision of the object (default: "max"=latest)
    find = function(object_type, object_data,
                    stage = 0, revision = "max") {
      return(.find(self$con, object_type=object_type, object_data=object_data,
                   stage = stage, revision = revision))
    },
    #' @description
    #' Find a focal person from an item_person.
    #'
    #' @param person_data An item_person in list form
    match_person = function(person_data) {
      return(.match_person(self$con, person_data,
                           only_most_recent = TRUE, only_active_stage = TRUE))
    }
  ),
  active = list(
    #' @field con
    #' ErudicionDB. Read-only.
    con = function() {
        private$pool
    }
  )
)
