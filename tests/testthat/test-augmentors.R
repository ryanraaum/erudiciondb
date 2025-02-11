
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
