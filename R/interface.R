

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
      where_clauses <- c(where_clauses, glue::glue_sql("{`id`} ILIKE {identifiers[[id]]}", .con = connection))
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
  }
  result_df
}

.find_item_by_year_volume_page <- function(connection, year, volume, first_page) {
  result_df <- EMPTY_FIND_RESULT
  if (.this_exists(year) && .this_exists(volume) && .this_exists(first_page)) {
    search_query <- glue::glue_sql("
      SELECT item_id, stage, revision FROM items
      WHERE year(issued) = {year} AND volume ILIKE {as.character(volume)} AND page_first ILIKE {as.character(first_page)}",
                                   .con = connection
    )
    query_results <- DBI::dbSendQuery(connection, search_query)
    fetched_results <- DBI::dbFetch(query_results)
    if (nrow(fetched_results) > 0) {
      result_df <- fetched_results |>
        dplyr::mutate(similarity = 1, found_by = "year_volume_page")
    }
  }
  result_df
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
    if (nrow(found) == 0) {
      found <- .find_item_by_year_volume_page(
        connection = connection,
        year = lubridate::year(object_data$issued),
        volume = object_data$volume,
        first_page = object_data$page_first
      )
    }
  #   if (nrow(found) == 0) {
  #     found <- .db_find_item_by_title(
  #       title = object_data$title,
  #       con = connection
  #     )
  #   }
  #   if (nrow(found) == 0) {
  #     found <- .db_find_item_by_person(
  #       person_id = object_data$person_id,
  #       con = connection
  #     )
  #   }
  # } else if (object_type == "person_identifier") {
  #   found <- .db_find_person_identifier(object_data, connection)
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
