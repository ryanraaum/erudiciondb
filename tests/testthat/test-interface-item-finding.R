test_that("item can be found by identifiers that are part of citeproc schema", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    proto_new_item <- list(title="GenBank",
                           container_title="Nucleic Acids Research",
                           container_title_short="Nucleic Acids Res",
                           volume=44,
                           issue="D1",
                           page="D67-D72",
                           issued=as.Date("20151120", "%Y%m%d"),
                           doi="10.1093/nar/gkv1276",
                           pmid="26590407",
                           pmcid="PMC4702903"
    )
    new_item <- expect_no_condition(.new_object(testcon, "item", proto_new_item))
    new_item_id <- expect_no_condition(.insert_one(testcon, new_item))

    for (id in c("doi", "pmid", "pmcid")) {
      this_id <- proto_new_item[[id]]
      found_1 <- expect_no_error(.find_item_by_identifier(testcon,
                                                              doi=this_id,
                                                              pmid=this_id,
                                                              pmcid=this_id))
      expect_true(nrow(found_1) == 1)
      expect_true(found_1$item_id == new_item_id)

      found_2 <- expect_no_error(.find(testcon, "item", list(doi=this_id,
                                                                 pmid=this_id,
                                                                 pmcid=this_id)))
      expect_true(nrow(found_2) == 1)
      expect_true(found_2$item_id == new_item_id)
    }
  }
})

test_that("item can be found by year, volume, and page", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    proto_new_item <- list(title="GenBank",
                           container_title="Nucleic Acids Research",
                           container_title_short="Nucleic Acids Res",
                           volume=44,
                           issue="D1",
                           page_first="D67",
                           page="D67-D72",
                           issued=as.Date("20151120", "%Y%m%d"),
                           doi="10.1093/nar/gkv1276",
                           pmid="26590407",
                           pmcid="PMC4702903"
    )
    new_item <- expect_no_condition(.new_object(testcon, "item", proto_new_item))
    new_item_id <- expect_no_condition(.insert_one(testcon, new_item))

    found_1 <- expect_no_condition(.find_item_by_year_volume_page(testcon,
                                                                  year="2015",
                                                                  volume="44",
                                                                  first_page="D67"))
    found_2 <- expect_no_condition(.find(testcon, "item", list(issued=as.Date("20151120", "%Y%m%d"),
                                                               volume="44",
                                                               page_first="D67")))
    expect_true(nrow(found_2) == 1)
    expect_true(found_2$item_id == new_item_id)
  }
})


test_that("item can be found by title", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    proto_new_item <- list(title="GenBank",
                           container_title="Nucleic Acids Research",
                           container_title_short="Nucleic Acids Res",
                           volume=44,
                           issue="D1",
                           page_first="D67",
                           page="D67-D72",
                           issued=as.Date("20151120", "%Y%m%d"),
                           doi="10.1093/nar/gkv1276",
                           pmid="26590407",
                           pmcid="PMC4702903"
    )
    new_item <- expect_no_condition(.new_object(testcon, "item", proto_new_item))
    new_item_id <- expect_no_condition(.insert_one(testcon, new_item))

    found_1 <- expect_no_condition(.find_item_by_title(testcon, title="genbank"))
    found_2 <- expect_no_condition(.find(testcon, "item", list(title="genbank")))
    expect_true(nrow(found_2) == 1)
    expect_true(found_2$item_id == new_item_id)
  }
})

test_that("SQLite title search returns found_by metadata", {
  # Only test SQLite (this was the bug)
  testcon <- make_testcon("sqlite")
  expect_no_error(edb_create_tables(testcon))

  # Insert a test item
  proto_new_item <- list(
    title="Unique Test Title for Bug Fix",
    container_title="Test Journal"
  )
  new_item <- .new_object(testcon, "item", proto_new_item)
  new_item_id <- .insert_one(testcon, new_item)

  # Search by title using .find_item_by_title directly
  found <- .find_item_by_title(testcon, title="Unique Test")

  # Verify found_by column exists and is set to "title"
  expect_true("found_by" %in% names(found))
  expect_true(nrow(found) > 0)
  expect_equal(found$found_by[1], "title")

  # Also test via .find() wrapper
  found2 <- .find(testcon, "item", list(title="Unique Test"))
  expect_true("found_by" %in% names(found2))
  expect_equal(found2$found_by[1], "title")
})

test_that(".find_item_by_title handles empty string gracefully", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Insert a test item
    proto_new_item <- list(
      title = "Test Article",
      container_title = "Test Journal"
    )
    new_item <- .new_object(testcon, "item", proto_new_item)
    new_item_id <- .insert_one(testcon, new_item)

    # Search with empty string should return no results
    found_empty <- .find_item_by_title(testcon, title = "")
    expect_equal(nrow(found_empty), 0)

    # Search with whitespace-only string should also return no results
    found_whitespace <- .find_item_by_title(testcon, title = "   ")
    expect_equal(nrow(found_whitespace), 0)

    # Verify valid search still works
    found_valid <- .find_item_by_title(testcon, title = "Test")
    expect_true(nrow(found_valid) > 0)
  }
})

test_that(".find_item_by_title handles NULL and NA", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Search with NULL should return no results
    found_null <- .find_item_by_title(testcon, title = NULL)
    expect_equal(nrow(found_null), 0)

    # Search with NA should return no results
    found_na <- .find_item_by_title(testcon, title = NA)
    expect_equal(nrow(found_na), 0)
  }
})


test_that("item can be found by person", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    proto_new_item <- list(title="GenBank",
                           container_title="Nucleic Acids Research",
                           container_title_short="Nucleic Acids Res",
                           volume=44,
                           issue="D1",
                           page_first="D67",
                           page="D67-D72",
                           issued=as.Date("20151120", "%Y%m%d"),
                           doi="10.1093/nar/gkv1276",
                           pmid="26590407",
                           pmcid="PMC4702903"
    )
    new_item <- expect_no_condition(.new_object(testcon, "item", proto_new_item))
    new_item_id <- expect_no_condition(.insert_one(testcon, new_item))

    proto_personlist <- list(item_id=new_item_id,
                             personlist_type="author")
    new_personlist <- expect_no_condition(.new_object(testcon, "personlist", proto_personlist))
    new_personlist_id <- expect_no_condition(.insert_one(testcon, new_personlist))

    proto_person <- list(surnames="Clark",
                         primary_given_names="Karen")
    new_person <- expect_no_condition(.new_object(testcon, "person", proto_person))
    new_person_id <- expect_no_condition(.insert_one(testcon, new_person))

    proto_item_person <- list(personlist_id=new_personlist_id,
                              person_id=new_person_id,
                              family="Clark",
                              given="Karen")
    new_item_person <- expect_no_condition(.new_object(testcon, "item_person", proto_item_person))
    new_item_person_id <- expect_no_condition(.insert_one(testcon, new_item_person))

    found_1 <- expect_no_condition(.find_item_by_person(testcon, person_id=new_person_id))
    expect_true(nrow(found_1) == 1)
    expect_true(found_1$item_id == new_item_id)

    found_2 <- expect_no_condition(.find(testcon, "item", list(title="genbank")))
    expect_true(nrow(found_2) == 1)
    expect_true(found_2$item_id == new_item_id)
  }
})

test_that("item can be found through object interface", {
  for (db in supported_databases()) {
    testdbobj <- expect_no_error(make_testdbobj(db))
    expect_no_error(edb_create_tables(testdbobj$con))

    proto_new_item <- list(title="GenBank",
                           container_title="Nucleic Acids Research",
                           container_title_short="Nucleic Acids Res",
                           volume=44,
                           issue="D1",
                           page_first="D67",
                           page="D67-D72",
                           issued=as.Date("20151120", "%Y%m%d"),
                           doi="10.1093/nar/gkv1276",
                           pmid="26590407",
                           pmcid="PMC4702903"
    )
    new_item <- expect_no_condition(testdbobj$new_object("item", proto_new_item))
    new_item_id <- expect_no_condition(testdbobj$insert_object(new_item))

    proto_personlist <- list(item_id=new_item_id,
                             personlist_type="author")
    new_personlist <- expect_no_condition(testdbobj$new_object("personlist", proto_personlist))
    new_personlist_id <- expect_no_condition(testdbobj$insert_object(new_personlist))

    proto_person <- list(surnames="Clark",
                         primary_given_names="Karen")
    new_person <- expect_no_condition(testdbobj$new_object("person", proto_person))
    new_person_id <- expect_no_error(testdbobj$insert_object(new_person))

    proto_item_person <- list(personlist_id=new_personlist_id,
                              person_id=new_person_id,
                              family="Clark",
                              given="Karen")
    new_item_person <- expect_no_condition(testdbobj$new_object("item_person", proto_item_person))
    new_item_person_id <- expect_no_condition(testdbobj$insert_object(new_item_person))

    found_1 <- expect_no_condition(testdbobj$find("item", new_item))
    expect_true(nrow(found_1) == 1)
    expect_true(found_1$item_id == new_item_id)

  }
})

test_that("item that does not exist is not found through object interface", {
  for (db in supported_databases()) {
    testdbobj <- expect_no_error(make_testdbobj(db))
    expect_no_error(edb_create_tables(testdbobj$con))

    item_w_identifiers <- list(doi="10.1093/nar/gkv1276",
                           pmid="26590407",
                           pmcid="PMC4702903")

    found_1 <- expect_no_error(testdbobj$find("item", item_w_identifiers))
    expect_true(nrow(found_1) == 0)

    item_w_year_volume_page <- list(volume=44,
                           issue="D1",
                           page_first="D67",
                           page="D67-D72",
                           issued=as.Date("20151120", "%Y%m%d"))

    found_2 <- expect_no_error(testdbobj$find("item", item_w_year_volume_page))
    expect_true(nrow(found_2) == 0)

    item_w_title <- list(title="GenBank")

    found_3 <- expect_no_error(testdbobj$find("item", item_w_title))
    expect_true(nrow(found_3) == 0)

  }
})


test_that("person_identifier can be found", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    proto_person_identifier <- list(person_id = uuid::UUIDgenerate(),
                                    id_type="testing_identifier",
                                    id_value="aBcD123/eFF.25")
    new_person_identifier <- expect_no_condition(.new_object(testcon, "person_identifier",
                                                             proto_person_identifier,
                                                             augment_function = .augment_person_identifier))
    new_person_identifier_id <- expect_no_condition(.insert_one(testcon, new_person_identifier))

    found_by_id <- expect_no_condition(.find_person_identifier(testcon, list(id_value="aBcD123/eFF.25")))
    expect_true(nrow(found_by_id) == 1)
    expect_true(found_by_id$person_identifier_id == new_person_identifier_id)
    found_by_id_2 <- expect_no_condition(.find(testcon, "person_identifier", list(id_value="aBcD123/eFF.25")))
    expect_true(nrow(found_by_id_2) == 1)
    expect_true(found_by_id_2$person_identifier_id == new_person_identifier_id)

    found_by_type_and_person <- expect_no_condition(.find_person_identifier(testcon, list(id_type="testing_identifier",
                                                                                          person_id=proto_person_identifier$person_id)))
    expect_true(nrow(found_by_type_and_person) == 1)
    expect_true(found_by_type_and_person$person_identifier_id == new_person_identifier_id)
    found_by_type_and_person_2 <- expect_no_condition(.find(testcon, "person_identifier", list(id_type="testing_identifier",
                                                                                          person_id=proto_person_identifier$person_id)))
    expect_true(nrow(found_by_type_and_person_2) == 1)
    expect_true(found_by_type_and_person_2$person_identifier_id == new_person_identifier_id)

    found_by_person_id <- expect_no_condition(.find_person_identifier(testcon, list(person_id=proto_person_identifier$person_id)))
    expect_true(nrow(found_by_person_id) == 1)
    expect_true(found_by_person_id$person_identifier_id == new_person_identifier_id)
    found_by_person_id_2 <- expect_no_condition(.find(testcon, "person_identifier", list(person_id=proto_person_identifier$person_id)))
    expect_true(nrow(found_by_person_id_2) == 1)
    expect_true(found_by_person_id_2$person_identifier_id == new_person_identifier_id)

  }
})

test_that("find will return any direct match", {
  for (db in supported_databases()) {
    testdbobj <- expect_no_error(make_testdbobj(db))
    expect_no_error(edb_create_tables(testdbobj$con))

    a_person <- list(primary_given_names = "Georg", surnames = "von Trapp")

    person_id <- expect_no_error(testdbobj$insert_new_object("person", a_person))

    found_1 <- expect_no_error(testdbobj$find("person", a_person))
    expect_equal(person_id, found_1$person_id)

  }
})
