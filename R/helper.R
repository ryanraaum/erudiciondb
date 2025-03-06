
## data helpers

# not vectorized; always returns a single value
.this_exists <- function(x) {
  !(all(is.na(x)) || all(is.null(x)) || length(x) == 0)
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

.word_from_title <- function(title, n=1) {
  title_words <- tolower(strsplit(title, "\\s+")[[1]])
  is_stop_word <- title_words %in% stopwords::stopwords()
  nonstop_title_words <- title_words[!is_stop_word]
  stringr::str_to_title(nonstop_title_words)[seq_len(n)]
}

.make_citekey <- function(surname, year, title, n=1) {
  surname <- stringi::stri_trans_general(surname, id = "Latin-ASCII")
  surname <- strsplit(surname, "\\W+")[[1]]
  surname <- paste(surname, collapse="")
  title <- stringi::stri_trans_general(title, id = "Latin-ASCII")
  title <- .word_from_title(title, n=n)
  title <- paste(title, collapse="")
  paste0(surname, year, title)
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
    dbobj <- .erudicion_db$new(list(drv=RSQLite::SQLite(), dbdir=":memory:"))
  } else if (dbtype == "duckdb") {
    dbobj <- .erudicion_db$new(list(drv=duckdb::duckdb(), dbdir=":memory:"))
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
