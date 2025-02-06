
## data helpers

# starting point derived from https://github.com/rijpma/capelinker (no LICENSE)
# - among other changes, now
#   - handles hyphenated names
#   - handles accented names
.initials <- function(strings, return_NA_on_empty = FALSE){
  out = stringi::stri_extract_all_regex(
    str = strings,
    pattern = "^[A-Za-zÀ-ÖØ-öø-ÿ]|(?:\\-)[A-Za-zÀ-ÖØ-öø-ÿ]|\\s[A-Za-zÀ-ÖØ-öø-ÿ]|[.][A-Za-zÀ-ÖØ-öø-ÿ]",
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

