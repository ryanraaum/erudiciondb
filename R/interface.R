

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

# object exists in the database, but will only exist here after it is pulled
#   so need to tell the function what kind of object to pull from the db
# `by` for searching by something other than the core object id
.retrieve <- function(connection, object_type, object_id, by=NULL, as_list=TRUE) {
  table_name <- glue::glue("{object_type}s")
  if (is.null(by)) {
    id_name <- glue::glue("{object_type}_id")
  } else {
    id_name <- by
  }
  object <- dplyr::tbl(connection, table_name) |>
    dplyr::filter(!!rlang::sym(id_name) %in% object_id) |>
    dplyr::collect() |>
    dplyr::mutate(object_type = object_type)

  if (!as_list) { return(object) }
  apply(object, 1, as.list)
}

# this is a nothing function that has no use other than to stop the
# R package build check function complaining about unused dbplyr
# (that needs to be loaded for using dplyr with databases,
#  but isn't actually directly called)
.make_check_dbplyr_note_go_away <- function(con) {
  dbplyr::dbplyr_edition(con)
}
