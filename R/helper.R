
## data helpers

#' Extract initials from name strings
#'
#' Extracts initials from given names, handling hyphens, periods, and accented
#' characters. Used in person matching for initials + surname matching strategy.
#'
#' @param strings Character vector of name strings
#' @param return_NA_on_empty Return NA for empty strings (TRUE) or "" (FALSE, default)
#'
#' @return Character vector of initials (e.g., "J.R." becomes "JR")
#'
#' @details
#' Extracts first letter of each word, handling:
#' - Word boundaries (spaces)
#' - Hyphens (e.g., "Jean-Paul" → "JP")
#' - Periods (e.g., "J.R." → "JR")
#' - Accented characters (Latin-1 supplement range)
#'
#' Starting point derived from https://github.com/rijpma/capelinker (no LICENSE)
#'
#' @note
#' Single-character parts are filtered out in `.shortest_distance()` when used
#' for name matching. The return_NA_on_empty parameter controls behavior for
#' empty input strings.
#'
#' @keywords internal
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

#' Split names into word parts
#'
#' Splits a name string on non-word characters and filters out empty parts.
#'
#' @param names Character string (single name, despite parameter name)
#'
#' @return Character vector of name parts with non-zero length
#'
#' @details
#' Splits on `\\W` (non-word characters: spaces, hyphens, periods, etc.) and
#' removes empty strings. Used as preprocessing for `.shortest_distance()` and
#' `.name_distance()` fuzzy matching functions.
#'
#' @keywords internal
.name_parts <- function(names) {
  split_name <- stringr::str_split(names, pattern="\\W")[[1]]
  split_name[nchar(split_name) > 0]
}

#' Calculate minimum Jaro-Winkler distance between name part sequences
#'
#' Compares two sets of name parts using a sliding window approach to find the
#' best alignment, returning the minimum average Jaro-Winkler distance.
#'
#' @param found_parts Character vector of name parts from search query
#' @param target_parts Character vector of name parts from database record
#'
#' @return Numeric: minimum average Jaro-Winkler distance (0 = perfect match, 1 = no match)
#'
#' @details
#' Algorithm:
#' 1. Filter out single-character parts (initials) from both inputs
#' 2. Return 1 (no match) if either input becomes empty after filtering
#' 3. If found_parts has one element, return minimum distance to any target part
#' 4. Use sliding window to compare found_parts to successive subsets of target_parts
#' 5. Return minimum average distance across all alignments
#'
#' The sliding window handles cases where names have different numbers of parts
#' (e.g., "Jean Paul Smith" vs "Jean Smith") by finding the best substring match.
#'
#' @note
#' Used by `.name_distance()` for fuzzy person name matching. Single-character
#' filtering ensures initials don't dominate the distance calculation.
#'
#' @keywords internal
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

#' Calculate Jaro-Winkler distance between a name and multiple target names
#'
#' Vectorized wrapper around `.shortest_distance()` that compares one search name
#' to multiple database names, splitting into parts and finding best alignments.
#'
#' @param found_name Character string: single name from search query
#' @param target_names Character vector: names from database to compare against
#'
#' @return Numeric vector: distance for each target name (0 = perfect, 1 = no match)
#'
#' @details
#' For each target name:
#' 1. Split both names into parts using `.name_parts()`
#' 2. Calculate minimum distance using `.shortest_distance()` sliding window
#' 3. Return vector of distances aligned with target_names
#'
#' Used in `.match_person()` for approximate name matching (4th strategy).
#' Threshold for match: distance < 0.05 for both given and family names.
#'
#' @keywords internal
.name_distance <- function(found_name, target_names) {
  found_name_parts <- .name_parts(found_name)
  target_name_parts <- purrr::map(target_names, .name_parts)
  name_distances <- purrr::map(target_name_parts, \(x) .shortest_distance(found_name_parts, x))
  unlist(name_distances)
}

#' Extract significant words from title for citation keys
#'
#' Extracts the first N non-stopword words from a title, used in citation key
#' generation to create readable keys like "Smith2020Example".
#'
#' @param title Character string: article/book title
#' @param n Number of words to extract (default: 1)
#'
#' @return Character vector of N title words in Title Case
#'
#' @details
#' Processing steps:
#' 1. Convert title to lowercase and split on whitespace
#' 2. Filter out stopwords using stopwords::stopwords() (common words like "the", "a", etc.)
#' 3. Select first N non-stopword words
#' 4. Convert to Title Case for readability
#'
#' Used by `.make_citekey()` to generate citation keys.
#'
#' @note
#' Stopword filtering ensures meaningful words appear in citation keys rather
#' than articles/prepositions.
#'
#' @keywords internal
.word_from_title <- function(title, n=1) {
  title_words <- tolower(strsplit(title, "\\s+")[[1]])
  is_stop_word <- title_words %in% stopwords::stopwords()
  nonstop_title_words <- title_words[!is_stop_word]
  stringr::str_to_title(nonstop_title_words)[seq_len(n)]
}

#' Generate citation key from surname, year, and title
#'
#' Creates a citation key in format "SurnameYearTitle" (e.g., "Smith2020Example")
#' for use in LaTeX/Markdown citations.
#'
#' @param surname Author surname (or institutional name)
#' @param year Publication year
#' @param title Article/book title
#' @param n Number of title words to include (default: 1)
#'
#' @return Character string citation key
#'
#' @details
#' Processing steps:
#' 1. Surname: Convert to ASCII, remove non-word characters, collapse
#' 2. Year: Use as-is (converted to string)
#' 3. Title: Convert to ASCII, extract N non-stopwords, collapse
#' 4. Concatenate: SurnameYearTitle
#'
#' Handles NA values by substituting empty strings. ASCII conversion ensures
#' LaTeX compatibility (e.g., "García" → "Garcia").
#'
#' @note
#' Used by `insert_new_item()` with collision detection via `.citekey_exists_in_db()`.
#' If collision detected, n is incremented or numeric suffix added.
#'
#' @keywords internal
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

item_date_columns <- c(
  "accessed",
  "available_date",
  "event_date",
  "issued",
  "original_date",
  "submitted"
)

#' Select best surname for citation key from item data
#'
#' Extracts the surname of the first creator from the highest-priority personlist
#' (author > editor > translator > etc.) for citation key generation.
#'
#' @param item_data Item object from repo2cp parsers (has item + personlist fields)
#'
#' @return Character string: surname or NA if none available
#'
#' @details
#' Algorithm:
#' 1. Find which personlist types exist in item_data (author, editor, etc.)
#' 2. Select highest priority type using valid_personlist_types ordering
#' 3. Get first person from that list
#' 4. Return family name (or literal name for institutional authors, or NA)
#'
#' Priority order defined in valid_personlist_types (author first, then editor, etc.).
#'
#' @note
#' Used by citation key generation in `insert_new_item()`. Returns NA if no
#' personlists exist or first person has no name.
#'
#' @keywords internal
.select_surname <- function(item_data) {
  personlists <- base::intersect(valid_personlist_types, names(item_data))

  if (length(personlists) == 0) {
    return(NA)
  }

  plists_order <- match(personlists, valid_personlist_types)
  which_preferred <- which(plists_order == min(plists_order))
  preferred <- personlists[which_preferred]

  # Check if the personlist is empty before accessing [[1]]
  if (length(item_data[[preferred]]) == 0) {
    return(NA)
  }

  first_person <- item_data[[preferred]][[1]]

  if (is.null(first_person)) {
    return(NA)
  }

  if (aidr::this_exists(first_person$family)) { return(first_person$family) }
  if (aidr::this_exists(first_person$literal)) { return(first_person$literal) }
  return(NA)
}

#' Extract publication year from item data for citation key
#'
#' Extracts the year component from the issued date field of an item.
#'
#' @param item_data Item object from repo2cp parsers (has item$issued field)
#'
#' @return Integer year or NA if issued date missing
#'
#' @details
#' Simple wrapper that extracts year from item$issued date using lubridate::year().
#' Exists as separate function to provide identifiable error source if date parsing
#' fails during citation key generation.
#'
#' @keywords internal
.select_year <- function(item_data) {
  if (aidr::this_exists(item_data$item$issued)) { return(lubridate::year(item_data$item$issued))}
  return(NA)
}

#' Extract title from item data for citation key
#'
#' Extracts the title field from an item for citation key generation.
#'
#' @param item_data Item object from repo2cp parsers (has item$title field)
#'
#' @return Character string title or NA if missing
#'
#' @details
#' Simple accessor function. Exists as separate function to provide identifiable
#' error source if title is missing during citation key generation (though this
#' is rare for properly formed items).
#'
#' @keywords internal
.select_title <- function(item_data) {
  if (aidr::this_exists(item_data$item$title)) { return(item_data$item$title)}
  return(NA)
}

#' Safely update object field value
#'
#' Returns new value if valid, otherwise preserves existing value. Used in
#' `.revise_object()` to handle NULL/NA updates gracefully.
#'
#' @param this_object Object being updated
#' @param this_variable Field name being updated
#' @param this_value New value to set
#'
#' @return New value if not NULL/NA, otherwise existing value from object
#'
#' @details
#' Simple helper that prevents NULL or NA from overwriting existing valid values.
#' If new value is NULL or NA, returns the current value from the object.
#' Otherwise returns the new value.
#'
#' @keywords internal
.update_it <- function(this_object, this_variable, this_value) {
  if (is.null(this_value) || is.na(this_value)) {
    return(this_object[[this_variable]])
  }
  this_value
}

#' Remove NA values from each list element
#'
#' Filters out NA values from each element of a list, used in CSL bibliography
#' generation to clean data before JSON export.
#'
#' @param l List to filter
#'
#' @return List with NA values removed from each element
#'
#' @keywords internal
.lapply_filter_na <- function(l) {
  lapply(l, function(x) x[!is.na(x)])
}

#' Remove NA values from a list
#'
#' Keeps only non-NA elements in a list, used in CSL bibliography generation.
#'
#' @param l List to filter
#'
#' @return List with only non-NA elements
#'
#' @keywords internal
.filter_na <- function(l) {
  purrr::keep(l, \(x) !is.na(x))
}

.filter_empty <- function(l) {
  lapply(l, function(x) x[length(x) > 0])
}

#' Identify internal metadata fields
#'
#' Determines which list elements are internal metadata (IDs, timestamps, etc.)
#' rather than bibliographic content for CSL export.
#'
#' @param l List to check
#' @param keep Character vector of field names to NOT consider internal (optional)
#'
#' @return Logical vector indicating which elements are internal
#'
#' @details
#' Internal fields are:
#' - Fields ending in "_id" (UUIDs)
#' - position, object_type, created, stage, revision
#'
#' The keep parameter allows exceptions (e.g., keep="person_id" to preserve
#' focal person tracking in CSL output).
#'
#' @keywords internal
.is_internal <- function(l, keep=NULL) {
  decision <- stringr::str_detect(names(l), "_id$") | names(l) %in% c("position", "object_type", "created", "stage", "revision")
  if (!is.null(keep)) {
    decision[which(names(l) %in% keep)] <- FALSE
  }
  decision
}

#' Remove internal metadata from each list element
#'
#' Filters out internal fields from each element of a list for CSL export.
#'
#' @param l List to filter
#'
#' @return List with internal fields removed from each element
#'
#' @keywords internal
.lapply_filter_internal <- function(l) {
  lapply(l, function(x) x[!.is_internal(x)])
}

#' Remove internal metadata fields from a list
#'
#' Filters out internal fields from a list for CSL bibliography export.
#'
#' @param l List to filter
#' @param keep Character vector of field names to preserve (optional)
#'
#' @return List with internal fields removed (except those in keep)
#'
#' @keywords internal
.filter_internal <- function(l, keep=NULL) {
  if (!is.null(keep) && !is.character(keep)) {
    stop("keep parameter must be a character vector")
  }
  l[!.is_internal(l, keep)]
}

#' Rename list element by pattern matching
#'
#' Renames list elements that match a pattern, used in CSL conversion to rename
#' snake_case fields to hyphenated CSL field names.
#'
#' @param l List to modify
#' @param from_name Pattern to match (regex)
#' @param to_name New name to set
#'
#' @return List with matching elements renamed
#'
#' @details
#' Finds all elements whose names match from_name pattern and renames them to
#' to_name. Used extensively in `.biblio_items_to_csl_list()` to convert field
#' names like "container_title" to "container-title".
#'
#' @keywords internal
.rename_element <- function(l, from_name, to_name) {
  targets <- which(stringr::str_detect(names(l), from_name))
  names(l)[targets] <- to_name
  l
}

## test helpers

#' Create test database connection
#'
#' Creates an in-memory database connection for testing, with automatic cleanup
#' via withr::defer().
#'
#' @param dbtype Database type: "sqlite" or "duckdb" (default: "sqlite")
#' @param env Environment for deferred cleanup (default: parent.frame())
#'
#' @return Database connection (SQLiteConnection or duckdb_connection)
#'
#' @details
#' Creates in-memory database (:memory:) that is automatically disconnected
#' when the calling environment exits. Used in test files for temporary databases.
#'
#' @keywords internal
#' @export
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

#' Create test database pool
#'
#' Creates an in-memory database pool for testing, with automatic cleanup
#' via withr::defer().
#'
#' @param dbtype Database type: "sqlite" or "duckdb" (default: "sqlite")
#' @param env Environment for deferred cleanup (default: parent.frame())
#'
#' @return Database pool (Pool object)
#'
#' @details
#' Creates in-memory database pool (:memory:) that is automatically closed
#' when the calling environment exits. Used in test files for temporary pooled
#' connections.
#'
#' @keywords internal
#' @export
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

#' Create test ErudicionDB object
#'
#' Creates an ErudicionDB object with in-memory database for testing.
#'
#' @param dbtype Database type: "sqlite" or "duckdb" (default: "sqlite")
#' @param env Environment for context (default: parent.frame())
#'
#' @return ErudicionDB R6 object
#'
#' @details
#' Creates in-memory database-backed ErudicionDB object for testing. Note that
#' automatic cleanup is currently disabled (commented out) - caller should
#' manually disconnect if needed.
#'
#' @keywords internal
#' @export
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

#' Get table column names
#'
#' Wrapper around DBI::dbListFields for test readability.
#'
#' @param con Database connection
#' @param table_name Name of table
#'
#' @return Character vector of column names
#'
#' @keywords internal
#' @export
table_columns <- function(con, table_name) {
  DBI::dbListFields(con, table_name)
}

#' Get list of supported database types
#'
#' Returns vector of supported database types for parameterized testing.
#'
#' @param just_this_one Optionally test only this database type (default: NULL = all)
#'
#' @return Character vector: c("duckdb", "sqlite") or single type
#'
#' @keywords internal
#' @export
supported_databases <- function(just_this_one=NULL) {
  if (is.null(just_this_one)) {return(c("duckdb", "sqlite"))}
  return(just_this_one)
}

#' Check if connection is SQLite
#'
#' Determines if a database connection or pool is SQLite (vs DuckDB).
#'
#' @param conn Database connection or pool
#'
#' @return Logical: TRUE if SQLite, FALSE otherwise
#'
#' @details
#' Handles both Pool objects (checks objClass) and direct connections (checks class).
#' Used to select database-specific SQL syntax (LIKE vs ILIKE, date functions, etc.).
#'
#' @keywords internal
#' @export
is_sqlite_connection <- function(conn) {
  if (inherits(conn, "Pool")) {
    return("SQLiteConnection" %in% conn$objClass)
  }
  return(isa(conn, "SQLiteConnection"))
}
