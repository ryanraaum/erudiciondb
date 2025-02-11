
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

table_columns <- function(con, table_name) {
  DBI::dbListFields(con, table_name)
}

supported_databases <- function(just_this_one=NULL) {
  if (is.null(just_this_one)) {return(c("duckdb", "sqlite"))}
  return(just_this_one)
}
