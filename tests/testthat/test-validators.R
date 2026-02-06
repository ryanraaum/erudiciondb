
test_that(".validate_person works", {
  # these should pass
  person_has_primary_given_name <- list(primary_given_names="Tammy")
  person_has_other_given_name <- list(other_given_names="Sue")
  person_has_surname <- list(surnames="Sweet")
  expect_no_condition(.validate_person(person_has_primary_given_name))
  expect_no_condition(.validate_person(person_has_other_given_name))
  expect_no_condition(.validate_person(person_has_surname))

  # these should fail
  person_has_no_names <- list()
  expect_error(.validate_person(person_has_no_names))
})

test_that(".validate_person rejects non-character types", {
  # Numeric primary_given_names
  person_numeric_primary <- list(primary_given_names = 123)
  expect_error(
    .validate_person(person_numeric_primary),
    "primary_given_names must be character, not numeric"
  )

  # Logical other_given_names
  person_logical_other <- list(other_given_names = TRUE)
  expect_error(
    .validate_person(person_logical_other),
    "other_given_names must be character, not logical"
  )

  # List surnames
  person_list_surname <- list(surnames = list("Smith"))
  expect_error(
    .validate_person(person_list_surname),
    "surnames must be character, not list"
  )

  # Numeric surnames
  person_numeric_surname <- list(surnames = 456)
  expect_error(
    .validate_person(person_numeric_surname),
    "surnames must be character, not numeric"
  )
})

test_that(".validate_person accepts valid character names", {
  # Valid names should still pass
  person_valid <- list(
    primary_given_names = "John",
    other_given_names = "Paul",
    surnames = "Smith"
  )
  expect_no_condition(.validate_person(person_valid))

  # Empty string is technically character, should pass type check
  # (semantic validation can happen elsewhere if needed)
  person_empty_string <- list(primary_given_names = "")
  expect_no_condition(.validate_person(person_empty_string))

  # Whitespace is character, should pass type check
  person_whitespace <- list(surnames = "   ")
  expect_no_condition(.validate_person(person_whitespace))
})

test_that(".validate_person handles NA and NULL correctly", {
  # NA values should be treated as "not provided" and skip type check
  person_with_na <- list(
    primary_given_names = "John",
    other_given_names = NA,
    surnames = NA
  )
  expect_no_condition(.validate_person(person_with_na))

  # NULL values should skip type check
  person_with_null <- list(
    primary_given_names = NULL,
    other_given_names = "Paul",
    surnames = NULL
  )
  expect_no_condition(.validate_person(person_with_null))

  # But all NA/NULL is still an error
  person_all_null <- list(
    primary_given_names = NULL,
    other_given_names = NULL,
    surnames = NULL
  )
  expect_error(.validate_person(person_all_null), "no core name entered")
})
