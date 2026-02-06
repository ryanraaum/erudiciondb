
VALIDATORS <- list()

.validate_person <- function(new_p) {
  # Check that at least one name field exists
  # Helper to check if value is missing (NULL or NA)
  is_missing <- function(x) is.null(x) || (length(x) == 1 && is.na(x))

  if (is_missing(new_p$primary_given_names) &&
      is_missing(new_p$other_given_names) &&
      is_missing(new_p$surnames)) {
    stop("no core name entered")
  }

  # Type validation for primary_given_names
  if (!is.na(new_p$primary_given_names) && !is.null(new_p$primary_given_names)) {
    if (!is.character(new_p$primary_given_names)) {
      stop("primary_given_names must be character, not ", class(new_p$primary_given_names)[1])
    }
  }

  # Type validation for other_given_names
  if (!is.na(new_p$other_given_names) && !is.null(new_p$other_given_names)) {
    if (!is.character(new_p$other_given_names)) {
      stop("other_given_names must be character, not ", class(new_p$other_given_names)[1])
    }
  }

  # Type validation for surnames
  if (!is.na(new_p$surnames) && !is.null(new_p$surnames)) {
    if (!is.character(new_p$surnames)) {
      stop("surnames must be character, not ", class(new_p$surnames)[1])
    }
  }
}
VALIDATORS[["person"]] <- .validate_person
