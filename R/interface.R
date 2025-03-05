

## ------ common functions

# object kind-of exists (i.e. there is a list that has some of the object values),
#   but all properties have not been filled out and it has not been validated;
#   so need to tell the function what kind of object to fill out and validate
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

.next_revision <- function(connection, object_type, object_id) {
  object_table <- glue::glue("{object_type}s")
  object_id_name <- glue::glue("{object_type}_id")
  current_max <- dplyr::tbl(connection, object_table) |>
    dplyr::filter(!!rlang::sym(object_id_name) == object_id) |>
    dplyr::pull("revision") |>
    max()
  current_max + 1
}

.revise_object <- function(connection, this_object, ...) {
  object_type <- this_object$object_type
  if (!.this_exists(object_type)) {
    stop("No `object_type` to be found - is this object properly formed?")
  }
  object_id_name <- glue::glue("{object_type}_id")
  if (!.this_exists(this_object[[object_id_name]])) {
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

# "update" = *revise* and then *insert* object into database
# - in this process, will also deactivate (stage = -1) current object in db
.update_object <- function(connection, this_object, ...) {
  revised_object <- .revise_object(connection, this_object, ...)
  DBI::dbBegin(connection)
  object_insertions_outcome <- try({
    revised_object_id <- .insert_one(connection, revised_object)
    unstaging_outcome <- .destage_one(connection, this_object)
    if (!unstaging_outcome) { stop("destaging error") }
  }, silent=TRUE)
  if (inherits(object_insertions_outcome, "try-error")) {
    DBI::dbRollback(connection)
    return(NULL)
  }
  DBI::dbCommit(connection)
  revised_object_id
}

# object exists and has been validated, so `object_type` is set in the object
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

# object exists in the database
.destage_one <- function(connection, object) {
  object_type <- object$object_type
  table_name <- glue::glue("{object_type}s")
  object_id_name <- glue::glue("{object_type}_id")
  object_id <- object[[object_id_name]]
  update_statement <- glue::glue_sql("UPDATE {`table_name`} SET stage = -1 WHERE {`object_id_name`} = {object_id} AND revision = {object$revision}",
                                     .con = connection)
  nrows_affected <- DBI::dbExecute(connection, update_statement)
  return(nrows_affected == 1)
}


# which revision to retrieve?
# NOTE that this choice is applied AFTER the `stage` filter
# - "max" (default) for only the most recent for the selected stage
# - "all" for all revisions at the selected stage
# - <number> to select a particular revision (that will need to exist at the selected stage)
.make_revision_filter <- function(rev) {
  this_filter <- function(.data) { dplyr::filter(.data, .data$revision == rev) }
  if (rev == "max") {
    this_filter <- function(.data) { dplyr::slice_max(.data, .data$revision, n=1) }
  } else if (rev == "all") {
    this_filter <- function(.data) { dplyr::filter(.data, TRUE) }
  }
  this_filter
}

# object exists in the database, but will only exist here after it is pulled
#   so need to tell the function what kind of object to pull from the db
# `by` for searching by something other than the core object id
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
    dplyr::filter(stage == stage) |>
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

.find_item_by_identifier <- function(connection, doi=NULL, pmid=NULL, pmcid=NULL) {
  result_df <- EMPTY_FIND_RESULT
  identifiers <- list(doi=doi, pmid=pmid, pmcid=pmcid)
  where_clauses <- c()
  for (id in names(identifiers)) {
    if (.this_exists(identifiers[[id]])) {
      if (isa(connection, "SQLiteConnection")) {
        where_clauses <- c(where_clauses, glue::glue_sql("{`id`} LIKE {identifiers[[id]]}", .con = connection))
      } else {
        where_clauses <- c(where_clauses, glue::glue_sql("{`id`} ILIKE {identifiers[[id]]}", .con = connection))
      }
    }
  }
  if (length(where_clauses) > 0) {
    combined_subclauses <- paste(where_clauses, collapse = " OR ")
    search_query <- glue::glue("SELECT item_id, stage, revision FROM items WHERE {combined_subclauses}")
    query_results <- DBI::dbSendQuery(connection, search_query)
    fetched_results <- DBI::dbFetch(query_results)
    if (nrow(fetched_results) > 0) {
      result_df <- fetched_results |>
        dplyr::mutate(similarity = 1, found_by = "identifier")
    }
    DBI::dbClearResult(query_results)
  }
  result_df
}

.find_item_by_year_volume_page <- function(connection, year, volume, first_page) {
  result_df <- EMPTY_FIND_RESULT
  if (.this_exists(year) && .this_exists(volume) && .this_exists(first_page)) {
    if (isa(connection, "SQLiteConnection")) {
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
    query_results <- DBI::dbSendQuery(connection, search_query)
    fetched_results <- DBI::dbFetch(query_results)
    if (nrow(fetched_results) > 0) {
      result_df <- fetched_results |>
        dplyr::mutate(similarity = 1, found_by = "year_volume_page")
    }
    DBI::dbClearResult(query_results)
  }
  result_df
}

.find_item_by_title <- function(connection, title) {
  result_df <- fetched_results <- EMPTY_FIND_RESULT

  if (.this_exists(title)) {
    search_title_nchar <- nchar(title)
    if (isa(connection, "SQLiteConnection")) {
      search_title = title

      # hack to stop package check from complaining
      item_id <- stage <- revision <- NULL

      result_df <- dplyr::tbl(connection, "items") |>
        dplyr::select(item_id, stage, revision, title) |>
        dplyr::collect() |>
        dplyr::mutate(similarity = 1-stringdist::stringdist(tolower(search_title), substring(tolower(title), 1, search_title_nchar), method="jw")) |>
        dplyr::select(-title)
    } else {
      search_query <- glue::glue_sql("
    SELECT
      item_id, stage, revision, jaro_winkler_similarity(lower({title}), substring(lower(title),1,{search_title_nchar})) as similarity
    FROM items WHERE similarity >= 0.9", .con = connection)
      query_results <- DBI::dbSendQuery(connection, search_query)
      fetched_results <- DBI::dbFetch(query_results)
      DBI::dbClearResult(query_results)
    }
    if (nrow(fetched_results) > 0) {
      result_df <- fetched_results |>
        dplyr::mutate(found_by = "title")
    }
  }
  result_df
}

.find_item_by_person <- function(connection, person_id) {
  result_df <- fetched_results <- EMPTY_FIND_RESULT
  this_person_id <- person_id
  item_id <- NULL # hack to silence `check()`

  if (.this_exists(person_id)) {
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

.find_person_identifier <- function(connection, object_data) {
  found <- tibble::tibble()
  id_value_uppercase <- id_type <- person_id <- NULL # to silence `check()`
  if (.this_exists(object_data$id_value)) {
    result_df <- dplyr::tbl(connection, "person_identifiers") |>
      dplyr::filter(id_value_uppercase == toupper(object_data$id_value)) |>
      dplyr::collect()
    if (nrow(result_df) > 0) {
      found <- result_df |>
        dplyr::mutate(similarity = 1, found_by = "identifier")
    }
  } else if (.this_exists(object_data$id_type) && .this_exists(object_data$person_id)) {
    result_df <- dplyr::tbl(connection, "person_identifiers") |>
      dplyr::filter(id_type == object_data$id_type, person_id == object_data$person_id) |>
      dplyr::collect()
    if (nrow(result_df) > 0) {
      found <- result_df |>
        dplyr::mutate(similarity = 1, found_by = "person_and_type")
    }
  } else if (.this_exists(object_data$person_id)) {
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
    if (nrow(found) == 0 && .this_exists(object_data$issued)) {
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

  found <- found |>
    dplyr::filter(stage == stage) |>
    .make_revision_filter(revision)()

  found
}


# this is a nothing function that has no use other than to stop the
# R package build check function complaining about unused dbplyr
# (that needs to be loaded for using dplyr with databases,
#  but isn't actually directly called)
.make_check_dbplyr_note_go_away <- function(con) {
  dbplyr::dbplyr_edition(con)
}

## Singleton DB Interface Object

.erudicion_db <- R6::R6Class(classname = "erudicion_db_object", # inherit = R6P::Singleton,
  public = list(
  pool = NULL,
  validators = NULL, #VALIDATORS,
  augmentors = AUGMENTORS,
  initialize = function(dbargs_list = DBARGS) {
    # super$initialize()
    self$establish_connection(dbargs_list)
  },
  establish_connection = function(dbargs_list) {
    if (is.null(self$pool) || !dbIsValid(self$pool)) {
      self$pool <- do.call(pool::"dbPool", dbargs_list)
    }
    invisible(self)
  },
  disconnect = function() {
    if (!is.null(self$pool)) {
      pool::poolClose(self$pool)
      self$pool <- NULL
    }
  },
  finalize = function() {
    self$disconnect()
  },
  tbl = function(tbl_name) {
    return(dplyr::tbl(self$pool, tbl_name))
  }
),
active = list(
  con = function(value) {
    if (missing(value)) {
      self$pool
    } else {
      stop("`$con` is read only", call. = FALSE)
    }
  }
))
