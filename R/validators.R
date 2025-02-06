
.validate_person <- function(new_p) {
  if (is.na(new_p$primary_given_names) &&
      is.na(new_p$other_given_names) &&
      is.na(new_p$surnames)) {
    stop("no core name entered")
  }
}
