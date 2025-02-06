
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
