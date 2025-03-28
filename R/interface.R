

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

.find_item_by_year_volume_page <- function(connection, year, volume, first_page) {
  result_df <- EMPTY_FIND_RESULT
  if (.this_exists(year) && .this_exists(volume) && .this_exists(first_page)) {
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

.find_item_by_title <- function(connection, title) {
  result_df <- fetched_results <- EMPTY_FIND_RESULT

  if (.this_exists(title)) {
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
    } else {
      search_query <- glue::glue_sql("
    SELECT
      item_id, stage, revision, jaro_winkler_similarity(lower({title}), substring(lower(title),1,{search_title_nchar})) as similarity
    FROM items WHERE similarity >= 0.9", .con = connection)
      fetched_results <- DBI::dbGetQuery(connection, search_query)
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

  if (nrow(found) == 0) {
    con <- pool::poolCheckout(connection)

    search_table <- dplyr::tbl(con, glue::glue("{object_type}s"))
    found <- search_table |>
      dplyr::inner_join(tibble::as_tibble(object_data), copy=TRUE) |>
      dplyr::collect()

    pool::poolReturn(con)
  }

  found <- found |>
    dplyr::filter(stage == stage) |>
    .make_revision_filter(revision)()

  found
}

.citekey_exists_in_db <- function(connection, citekey) {
  count_query <- glue::glue_sql("SELECT citation_key FROM items WHERE citation_key = {citekey}", .con = connection)
  count_result <- DBI::dbGetQuery(connection, count_query)
  this_count <- nrow(count_result)
  assertthat::assert_that(length(this_count) == 1, msg="Found something other than a single count value")
  this_count > 0
}

# find "person" from imported item person data
# - `pdata` is a list; relevant elements are `family`, `given`, `orcid`, and `literal`
#    (entries with a `literal` value and no `family` or `given` are institutional authors;
#       institutional authors will not be matched - persons table is only actual people)
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

  pdata$given_ascii = tolower(stringi::stri_trans_general(pdata$given, id = "Latin-ASCII"))
  pdata$family_ascii = tolower(stringi::stri_trans_general(pdata$family, id = "Latin-ASCII"))

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

  return(EMPTY_FIND_RESULT)
}


.biblio <- function(connection, item_id, format="json") {
  items <- .find(connection, "item", list(item_id=item_id))
  items <- apply(items, 1, as.list)
  if (length(items) == 0) { return(list()) }
  names(items) <- sapply(items, '[[', "item_id")
  items <- items |>
    .filter_na() |>
    .filter_internal() |>
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


  plists <- .biblio_personlist(connection, item_id)

  for (this_id in item_id) {
    items[[this_id]] <- c(items[[this_id]], plists[[this_id]])
  }

  if (format == "json") {
    names(items) <- NULL
    items.json <- jsonlite::toJSON(items, pretty=TRUE, auto_unbox=TRUE)
    return(items.json)
  }

  items
}


# do many at once
.biblio_personlist <- function(connection, item_id) {
  # just to silence check that doesn't understand piped tidy variables
  position <- NULL

  plists <- .retrieve(connection, "personlist", item_id, by="item_id", as_list=FALSE)
  if (nrow(plists) == 0) { return(list()) }

  persons <- .retrieve(connection, "item_person", plists$personlist_id, by="personlist_id", as_list=FALSE) |>
    dplyr::rename( "dropping-particle" = "dropping_particle",
                   "non-dropping-particle" = "non_dropping_particle",
                   "comma-suffix" = "comma_suffix",
                   "static-ordering" = "static_ordering",
                   "parse-names" = "parse_names")
  indexed_list <- split(persons, persons$personlist_id)  |>
    purrr::map(\(x) dplyr::arrange(x, position)) |>
    purrr::map(\(x) apply(x, 1, as.list)) |>
    purrr::map(.filter_na) |>
    purrr::map(.filter_internal)

  item_personlists <- list()
  for (this_item in unique(plists$item_id)) {
    this_subset <- plists |> dplyr::filter(item_id == this_item)
    this_plists <- this_subset$personlist_id
    this_type <- this_subset$personlist_type
    this_item_plist <- list()
    for (i in seq_len(nrow(this_subset))) {
      this_item_plist[[this_type[i]]] <- indexed_list[[this_plists[[i]]]]
    }
    item_personlists[[this_item]] <- this_item_plist
  }

  item_personlists
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
            if (.this_exists(found_person)) {
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
            if (.this_exists(found_person_issue)) {
              found_person_issue$object_id <- this_item_person_id
              .insert_new_object(conn, "issue", found_person_issue,
                                 validate_function = private$validators[["issue"]],
                                 augment_function = private$augmentors[["issue"]],
                                 stage=stage)
            }
            if (.this_exists(this_affiliation) && .this_exists(this_affiliation[[i]])) {
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
    #' Clean up after removal of ErudicionDB object
    finalize = function() {
      self$disconnect()
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
