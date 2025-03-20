
## data helpers

# not vectorized; always returns a single value
.this_exists <- function(x) {
  !(all(is.na(x)) || all(is.null(x)) || length(x) == 0)
}

.this_or_empty_string <- function(x) {
  ifelse(.this_exists(x), x, "")
}

# starting point derived from https://github.com/rijpma/capelinker (no LICENSE)
# - among other changes, now
#   - handles hyphenated names
#   - handles accented names
.initials <- function(strings, return_NA_on_empty = FALSE){
  out = stringi::stri_extract_all_regex(
    str = strings,
    pattern = "^[A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u00FF]|(?:\\-)[A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u00FF]|\\s[A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u00FF]|[.][A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u00FF]",
    simplify = FALSE,
    omit_no_match = TRUE) # if TRUE, empty list -> "" below

  out = lapply(out, stringi::stri_replace_all_regex, "[ .]", "")

  if(return_NA_on_empty){
    out[sapply(out, length) == 0] = NA
  } else if (!return_NA_on_empty){
    out[sapply(out, length) == 0] = ""
  }

  sapply(out, stringi::stri_join, collapse = "")
}

.name_parts <- function(names) {
  split_name <- stringr::str_split(names, pattern="\\W")[[1]]
  split_name[nchar(split_name) > 0]
}

.shortest_distance <- function(found_parts, target_parts) {
  found_parts <- found_parts[nchar(found_parts) > 1]
  target_parts <- target_parts[nchar(target_parts) > 1]
  if (length(found_parts) == 0) { return(1) }
  if (length(target_parts) == 0) { return(1) }
  if (length(found_parts) == 1) {
    return(min(stringdist::stringdist(found_parts, target_parts, method="jw")))
  }
  if (length(target_parts) >= length(found_parts)) {
    minimum_dist <- 1
    for (i in 1:(length(target_parts) - length(found_parts) + 1)) {
      # compare found parts to successive subsets of target parts
      current_distances <- vector("numeric", length(found_parts))
      for (j in seq_along(found_parts)) {
        current_distances[j] <- stringdist::stringdist(found_parts[j],
                                                       target_parts[i+j-1],
                                                       method="jw")
      }
      current_average <- mean(current_distances)
      if (current_average < minimum_dist) { minimum_dist <- current_average }
    }
    return(minimum_dist)
  }
  if (length(found_parts) > length(target_parts)) {
    minimum_dist <- 1
    for (i in 1:(length(found_parts) - length(target_parts) + 1)) {
      # compare found parts to successive subsets of target parts
      current_distances <- vector("numeric", length(target_parts))
      for (j in seq_along(target_parts)) {
        current_distances[j] <- stringdist::stringdist(target_parts[j],
                                                       found_parts[i+j-1],
                                                       method="jw")
      }
      current_average <- mean(current_distances)
      if (current_average < minimum_dist) { minimum_dist <- current_average }
    }
    return(minimum_dist)
  }
}

.name_distance <- function(found_name, target_names) {
  found_name_parts <- .name_parts(found_name)
  target_name_parts <- purrr::map(target_names, .name_parts)
  name_distances <- purrr::map(target_name_parts, \(x) .shortest_distance(found_name_parts, x))
  unlist(name_distances)
}

.word_from_title <- function(title, n=1) {
  title_words <- tolower(strsplit(title, "\\s+")[[1]])
  is_stop_word <- title_words %in% stopwords::stopwords()
  nonstop_title_words <- title_words[!is_stop_word]
  stringr::str_to_title(nonstop_title_words)[seq_len(n)]
}

.make_citekey <- function(surname, year, title, n=1) {
  if (!is.na(surname)) {
    surname <- stringi::stri_trans_general(surname, id = "Latin-ASCII")
    surname <- strsplit(surname, "\\W+")[[1]]
    surname <- paste(surname, collapse="")
  } else {
    surname <- ""
  }
  if (!is.na(title)) {
    title <- stringi::stri_trans_general(title, id = "Latin-ASCII")
    title <- .word_from_title(title, n=n)
    title <- paste(title, collapse="")
  } else {
    title <- ""
  }
  if (is.na(year)) { year <- "" }
  paste0(surname, year, title)
}

# these are ordered (badly and incompletely, by me) by preference for citation key label
valid_personlist_types <- c(
  "author",
  "editor",
  "translator",
  "director",
  "performer",
  "chair",
  "organizer",
  "collection-editor",
  "compiler",
  "composer",
  "container-author",
  "contributor",
  "curator",
  "editorial-director",
  "executive-producer",
  "guest",
  "host",
  "interviewer",
  "illustrator",
  "narrator",
  "original-author",
  "producer",
  "recipient",
  "reviewed-author",
  "script-writer",
  "series-creator"
)

# `item_data` is the object created by the `repo2cp` parsers
# here, we are selecting the "best" surname for the citekey
.select_surname <- function(item_data) {
  personlists <- base::intersect(valid_personlist_types, names(item_data))
  plists_order <- match(personlists, valid_personlist_types)
  which_preferred <- which(plists_order == min(plists_order))
  preferred <- personlists[which_preferred]
  first_person <- item_data[[preferred]][[1]]
  if (.this_exists(first_person$family)) { return(first_person$family) }
  if (.this_exists(first_person$literal)) { return(first_person$literal) }
  return(NA)
}

# `item_data` is the object created by the `repo2cp` parsers
# this doesn't really require a separate function, but it is here to
#   throw an identifiable error for items that might not have an `issued` date
.select_year <- function(item_data) {
  if (.this_exists(item_data$item$issued)) { return(lubridate::year(item_data$item$issued))}
  return(NA)
}

# `item_data` is the object created by the `repo2cp` parsers
# this doesn't really require a separate function, but it is here to
#   throw an identifiable error for items that might not have a `title`
#   (if that is possible?)
.select_title <- function(item_data) {
  if (.this_exists(item_data$item$title)) { return(item_data$item$title)}
  return(NA)
}

.update_it <- function(this_object, this_variable, this_value) {
  if (is.na(this_value)) {
    return(this_object[[this_variable]])
  }
  this_value
}



## test helpers

make_testcon <- function(dbtype="sqlite", env=parent.frame()) {
  if (dbtype == "sqlite") {
    testcon <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  } else if (dbtype == "duckdb") {
    testcon <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  } else {
    stop(glue::glue("not a known dbtype: '{dbtype}'"))
  }
  withr::defer(
    DBI::dbDisconnect(testcon),
    envir = env
  )
  testcon
}

make_testpool <- function(dbtype="sqlite", env=parent.frame()) {
  if (dbtype == "sqlite") {
    testcon <- pool::dbPool(RSQLite::SQLite(), dbdir=":memory:")
  } else if (dbtype == "duckdb") {
    testcon <- pool::dbPool(duckdb::duckdb(), dbdir=":memory:")
  } else {
    stop(glue::glue("not a known dbtype: '{dbtype}'"))
  }
  withr::defer(
    pool::poolClose(testcon),
    envir = env
  )
  testcon
}

make_testdbobj <- function(dbtype="sqlite", env=parent.frame()) {
  if (dbtype == "sqlite") {
    dbobj <- ErudicionDB$new(list(drv=RSQLite::SQLite(), dbdir=":memory:"))
  } else if (dbtype == "duckdb") {
    dbobj <- ErudicionDB$new(list(drv=duckdb::duckdb(), dbdir=":memory:"))
  } else {
    stop(glue::glue("not a known dbtype: '{dbtype}'"))
  }
  # withr::defer(
  #   dbobj$disconnect(),
  #   envir = env
  # )
  dbobj
}

table_columns <- function(con, table_name) {
  DBI::dbListFields(con, table_name)
}

supported_databases <- function(just_this_one=NULL) {
  if (is.null(just_this_one)) {return(c("duckdb", "sqlite"))}
  return(just_this_one)
}

is_sqlite_connection <- function(conn) {
  if (inherits(conn, "Pool")) {
    return("SQLiteConnection" %in% conn$objClass)
  }
  return(isa(conn, "SQLiteConnection"))
}
