
VALIDATORS <- list()

#' Validate person object before insertion
#'
#' Ensures person has at least one name field and validates name field types.
#' Currently the only implemented validator function.
#'
#' @param new_p Person object (list with person fields)
#'
#' @return NULL (throws error on validation failure)
#'
#' @details
#' Validation rules:
#' 1. At least one of primary_given_names, other_given_names, or surnames must be present
#' 2. If present and not NA/NULL, name fields must be character type
#'
#' Throws descriptive errors for:
#' - Missing all name fields: "no core name entered"
#' - Wrong type: "(field) must be character, not (type)"
#'
#' @note
#' This is the only validator currently implemented. Other object types pass
#' through without validation (validators list entry is NULL).
#'
#' @keywords internal
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
