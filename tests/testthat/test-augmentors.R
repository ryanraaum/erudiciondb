
test_that(".augment_person works", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    proto_new_person_1 <- list(primary_given_names = "Tammy",
                               other_given_names = "Sue",
                               surnames = "Sweetie")
    new_person_1 <- expect_no_condition(.new_object(testcon, "person", proto_new_person_1))

    augmented_person_1 <- expect_no_condition(.augment_person(new_person_1))

    expect_false(augmented_person_1$also_known_as)
    expect_false(augmented_person_1$comma_suffix)
    expect_equal(augmented_person_1$ascii_given_names, "tammy sue")
    expect_equal(augmented_person_1$ascii_surnames, "sweetie")
    expect_equal(augmented_person_1$short_referent, "Tammy")
    expect_equal(augmented_person_1$long_referent, "Tammy S Sweetie")
    expect_equal(augmented_person_1$sorting_referent, "sweetie")

    proto_new_person_2 <- list(primary_given_names = "Émily Sûe",
                               surnames = "Žoe")
    new_person_2 <- expect_no_condition(.new_object(testcon, "person", proto_new_person_2))

    augmented_person_2 <- expect_no_condition(.augment_person(new_person_2))

    expect_false(augmented_person_2$also_known_as)
    expect_false(augmented_person_2$comma_suffix)
    expect_equal(augmented_person_2$ascii_given_names, "emily sue")
    expect_equal(augmented_person_2$ascii_surnames, "zoe")
    expect_equal(augmented_person_2$short_referent, "Émily Sûe")
    expect_equal(augmented_person_2$long_referent, "Émily Sûe Žoe")
    expect_equal(augmented_person_2$sorting_referent, "zoe")

    proto_new_person_3 <- list(primary_given_names = "Juan",
                               non_dropping_particle = "de la",
                               surnames = "Roche")
    new_person_3 <- expect_no_condition(.new_object(testcon, "person", proto_new_person_3))

    augmented_person_3 <- expect_no_condition(.augment_person(new_person_3))

    expect_false(augmented_person_3$also_known_as)
    expect_false(augmented_person_3$comma_suffix)
    expect_equal(augmented_person_3$ascii_given_names, "juan")
    expect_equal(augmented_person_3$ascii_surnames, "roche")
    expect_equal(augmented_person_3$short_referent, "Juan")
    expect_equal(augmented_person_3$long_referent, "Juan de la Roche")
    expect_equal(augmented_person_3$sorting_referent, "de la roche")
  }
})


test_that(".augment_person_identifier works", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    proto_person_identifier <- list(person_id = uuid::UUIDgenerate(),
                                    id_type="testing_identifier",
                                    id_value="aBcD123/eFF.25")
    new_person_identifier <- expect_no_condition(.new_object(testcon, "person_identifier", proto_person_identifier))
    augmented_person_identifier <- expect_no_condition(.augment_person_identifier(new_person_identifier))

    expect_equal(augmented_person_identifier$id_value_uppercase, toupper(augmented_person_identifier$id_value))
  }
})


test_that(".augment_item_plus works", {
  item <- list(citation_key = NA,
               issued = as.Date("20151120", "%Y%m%d"),
               title = "GenBank")
  creator_person <- list(family="Clark", given="Karen")
  creator_institution <- list(literal="International HapMap Consortium")

  augmented_person <- expect_no_condition(.augment_item_plus(item, creator_person))
  expect_equal(augmented_person$citation_key, "Clark2015Genbank")
  augmented_institution <- expect_no_condition(.augment_item_plus(item, creator_institution))
  expect_equal(augmented_institution$citation_key, "InternationalHapMapConsortium2015Genbank")
})

test_that(".augment_item_plus raises error when issued date is missing", {
  item_no_date <- list(
    citation_key = NA,
    title = "GenBank"
    # issued is missing
  )
  creator <- list(family = "Clark", given = "Karen")

  expect_error(
    .augment_item_plus(item_no_date, creator),
    "No viable publication date",
    fixed = TRUE
  )
})

test_that(".augment_item_plus raises error when issued is NA", {
  item_na_date <- list(
    citation_key = NA,
    issued = NA,
    title = "GenBank"
  )
  creator <- list(family = "Clark", given = "Karen")

  expect_error(
    .augment_item_plus(item_na_date, creator),
    "No viable publication date",
    fixed = TRUE
  )
})

test_that(".augment_item_plus raises error when issued is NULL", {
  item_null_date <- list(
    citation_key = NA,
    issued = NULL,
    title = "GenBank"
  )
  creator <- list(family = "Clark", given = "Karen")

  expect_error(
    .augment_item_plus(item_null_date, creator),
    "No viable publication date",
    fixed = TRUE
  )
})

test_that(".augment_item_plus raises error when issued is not a Date object", {
  # Test character string
  item_string_date <- list(
    citation_key = NA,
    issued = "2024-01-01",
    title = "GenBank"
  )
  creator <- list(family = "Clark", given = "Karen")

  expect_error(
    .augment_item_plus(item_string_date, creator),
    "No viable publication date",
    fixed = TRUE
  )

  # Test numeric
  item_numeric_date <- list(
    citation_key = NA,
    issued = 20240101,
    title = "GenBank"
  )

  expect_error(
    .augment_item_plus(item_numeric_date, creator),
    "No viable publication date",
    fixed = TRUE
  )
})

test_that(".augment_item_plus works with valid Date object", {
  item_valid <- list(
    citation_key = NA,
    issued = as.Date("2015-11-20"),
    title = "GenBank"
  )
  creator <- list(family = "Clark", given = "Karen")

  result <- expect_no_error(.augment_item_plus(item_valid, creator))
  expect_equal(result$citation_key, "Clark2015Genbank")
})

