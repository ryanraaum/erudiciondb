
## Standalone Augmentors (no extra data required)

AUGMENTORS <- list()

#' Augment person object with computed fields
#'
#' Enriches person data with ASCII name variants, referents for display, and
#' sorting referent for alphabetization. Most complex augmentor function.
#'
#' @param new_p Person object (list with person fields)
#'
#' @return Person object with computed fields filled in
#'
#' @details
#' Computes fields if not already provided:
#'
#' **ASCII variants** (for fuzzy matching):
#' - `ascii_given_names`: Lowercase ASCII transliteration of combined primary + other given names
#' - `ascii_surnames`: Lowercase ASCII transliteration of surnames (excludes particles)
#'
#' **Referents** (for display):
#' - `short_referent`: Primary given names only (e.g., "Jean-Paul")
#' - `long_referent`: Full name with all components in order:
#'   prefix, primary given names, initials of other given names, particles, surnames, suffix
#'   (e.g., "Dr. Jean-Paul M. de Martínez, Jr.")
#' - `sorting_referent`: Lowercase ASCII surname with non-dropping particle
#'   (e.g., "de martinez" for "Martínez")
#'
#' **Flags**:
#' - `also_known_as`: Defaults to FALSE
#' - `comma_suffix`: Defaults to FALSE (controls comma before suffix in long_referent)
#'
#' @note
#' Sorting referent handles mononyms (people with only given names) by using
#' primary given name as sort key. Non-dropping particles are included in sort
#' (e.g., "de Gaulle" sorts under "D"), but dropping particles are not.
#'
#' @keywords internal
.augment_person <- function(new_p) {
  if (is.na(new_p$also_known_as)) {
    new_p$also_known_as <- FALSE
  }
  if (is.na(new_p$comma_suffix)) {
    new_p$comma_suffix <- FALSE
  }
  if (is.na(new_p$ascii_given_names)) {
    given_names <- c(new_p$primary_given_names, new_p$other_given_names)
    given_names <- given_names[!is.na(given_names)]
    given_names <- paste(given_names, collapse=" ")
    new_p$ascii_given_names <- tolower(stringi::stri_trans_general(given_names, id = "Latin-ASCII"))
  }
  if (is.na(new_p$ascii_surnames)) {
    # NOTE: not including particles, unclear if that is the right move
    new_p$ascii_surnames <- tolower(stringi::stri_trans_general(new_p$surnames, id = "Latin-ASCII"))
  }
  if (is.na(new_p$short_referent)) {
    new_p$short_referent <- new_p$primary_given_names
  }
  if (is.na(new_p$long_referent)) {
    all <- c(
      new_p$prefix,
      new_p$primary_given_names,
      .initials(new_p$other_given_names),
      new_p$dropping_particle,
      new_p$non_dropping_particle,
      new_p$surnames,
      ifelse(new_p$comma_suffix, ",", NA),
      new_p$suffix
    )
    all <- all[!is.na(all)]
    new_p$long_referent <- paste(all, collapse = " ") |>
      stringr::str_replace(" , ", ", ") # get rid of extra space before comma suffix if necessary
  }
  if (is.na(new_p$sorting_referent)) {
    if (is.na(new_p$surnames)) {
      # if no surnames, then mononym? so use given names as sorting?
      new_p$sorting_referent <- stringi::stri_trans_general(new_p$primary_given_names, "latin-ascii")
    } else {
      # include non-dropping particle
      sorting_surname <- c(new_p$non_dropping_particle, new_p$surnames)
      sorting_surname <- sorting_surname[!is.na(sorting_surname)]
      if (!is.na(new_p$non_dropping_particle) && endsWith(new_p$non_dropping_particle, "-")) {
        sorting_surname <- paste(sorting_surname, collapse="")
      } else {
        sorting_surname <- paste(sorting_surname, collapse=" ")
      }
      new_p$sorting_referent <- tolower(stringi::stri_trans_general(sorting_surname, "latin-ascii"))
    }
  }
  new_p
}
AUGMENTORS[["person"]] <- .augment_person

#' Augment person_identifier with uppercase variant
#'
#' Creates normalized uppercase version of identifier value for case-insensitive
#' searching.
#'
#' @param new_pid Person identifier object (list)
#'
#' @return Person identifier with id_value_uppercase computed
#'
#' @details
#' If id_value_uppercase is not already set and id_value exists, computes
#' id_value_uppercase = toupper(id_value). Used for case-insensitive matching
#' in `.find_person_identifier()`.
#'
#' @keywords internal
.augment_person_identifier <- function(new_pid) {
  if (is.na(new_pid$id_value_uppercase) && aidr::this_exists(new_pid$id_value)) {
    new_pid$id_value_uppercase <- toupper(new_pid$id_value)
  }
  new_pid
}
AUGMENTORS[["person_identifier"]] <- .augment_person_identifier


## Insertion Augmentors ("plus" extra data)

# `creator` is a list with item_person data
.augment_item_plus <- function(new_item, creator) {
  if (is.na(new_item$citation_key)) {

    citekey_surname <- creator$family
    if (is.null(citekey_surname)) { citekey_surname <- creator$literal }
    if (is.null(citekey_surname)) { stop("No viable creator name for citekey") }

    if (!aidr::this_exists(new_item$issued) || !lubridate::is.Date(new_item$issued)) {
      stop("No viable publication date")
    }
    citekey_year <- lubridate::year(new_item$issued)

    citekey_title <- new_item$title

    this_citekey <- .make_citekey(citekey_surname, citekey_year, citekey_title)
    new_item$citation_key <- this_citekey
  }

  new_item
}
