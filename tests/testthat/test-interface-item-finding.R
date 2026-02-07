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


# Tests for .citekey_exists_in_db() -------------------------------------------

test_that(".citekey_exists_in_db returns FALSE when citekey does not exist", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Check for non-existent citekey
    result <- expect_no_error(.citekey_exists_in_db(testcon, "Smith2020GenBank"))
    expect_false(result)

    # Check multiple non-existent citekeys
    expect_false(.citekey_exists_in_db(testcon, "Jones2021Nature"))
    expect_false(.citekey_exists_in_db(testcon, "Brown1999Science"))
  }
})

test_that(".citekey_exists_in_db returns TRUE when citekey exists", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Create an item with a citation_key
    proto_new_item <- list(
      title = "GenBank",
      container_title = "Nucleic Acids Research",
      issued = as.Date("2015-11-20"),
      citation_key = "Smith2020GenBank"
    )
    new_item <- expect_no_condition(.new_object(testcon, "item", proto_new_item))
    new_item_id <- expect_no_condition(.insert_one(testcon, new_item))

    # Check that the citekey exists
    result <- expect_no_error(.citekey_exists_in_db(testcon, "Smith2020GenBank"))
    expect_true(result)

    # Verify different citekey still returns FALSE
    expect_false(.citekey_exists_in_db(testcon, "Jones2020GenBank"))
  }
})

test_that(".citekey_exists_in_db is case-sensitive", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Create an item with lowercase citekey
    proto_new_item <- list(
      title = "Test Article",
      container_title = "Test Journal",
      issued = as.Date("2020-01-01"),
      citation_key = "smith2020test"
    )
    new_item <- .new_object(testcon, "item", proto_new_item)
    .insert_one(testcon, new_item)

    # Exact match should exist
    expect_true(.citekey_exists_in_db(testcon, "smith2020test"))

    # Different case should NOT exist (case-sensitive)
    expect_false(.citekey_exists_in_db(testcon, "Smith2020Test"))
    expect_false(.citekey_exists_in_db(testcon, "SMITH2020TEST"))
    expect_false(.citekey_exists_in_db(testcon, "Smith2020test"))
  }
})

test_that(".citekey_exists_in_db handles edge cases", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Empty string should return FALSE (no match)
    expect_false(.citekey_exists_in_db(testcon, ""))

    # Special characters
    proto_special <- list(
      title = "Test",
      issued = as.Date("2020-01-01"),
      citation_key = "Smith-2020_Test"
    )
    new_special <- .new_object(testcon, "item", proto_special)
    .insert_one(testcon, new_special)
    expect_true(.citekey_exists_in_db(testcon, "Smith-2020_Test"))

    # Numeric string as citekey
    proto_numeric <- list(
      title = "Article",
      issued = as.Date("2021-01-01"),
      citation_key = "123456"
    )
    new_numeric <- .new_object(testcon, "item", proto_numeric)
    .insert_one(testcon, new_numeric)
    expect_true(.citekey_exists_in_db(testcon, "123456"))

    # Citekey with spaces
    proto_spaces <- list(
      title = "Paper",
      issued = as.Date("2022-01-01"),
      citation_key = "Smith 2022 Paper"
    )
    new_spaces <- .new_object(testcon, "item", proto_spaces)
    .insert_one(testcon, new_spaces)
    expect_true(.citekey_exists_in_db(testcon, "Smith 2022 Paper"))
  }
})

test_that(".citekey_exists_in_db checks all revisions, not just active", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Create an item with a citation_key
    proto_item <- list(
      title = "Original Title",
      issued = as.Date("2020-01-01"),
      citation_key = "Test2020Original"
    )
    new_item <- .new_object(testcon, "item", proto_item)
    item_id <- .insert_one(testcon, new_item)

    # Verify citekey exists
    expect_true(.citekey_exists_in_db(testcon, "Test2020Original"))

    # Update the item (creates new revision, old one becomes stage=-1)
    # .retrieve() returns a list, extract first element
    updated_item <- .retrieve(testcon, "item", item_id)[[1]]
    updated_item$title <- "Updated Title"
    # .update_object() expects the object directly (not object_type as parameter)
    .update_object(testcon, updated_item)

    # The citation_key from the inactive revision should still be detected
    expect_true(.citekey_exists_in_db(testcon, "Test2020Original"))

    # Verify we now have 2 revisions in the database
    all_revisions <- DBI::dbGetQuery(
      testcon,
      glue::glue_sql("SELECT revision, stage FROM items WHERE item_id = {item_id}",
                     .con = testcon)
    )
    expect_equal(nrow(all_revisions), 2)
    expect_true(any(all_revisions$stage == 0))  # active revision
    expect_true(any(all_revisions$stage == -1)) # inactive revision
  }
})

test_that(".citekey_exists_in_db prevents collision in insert_new_object", {
  for (db in supported_databases()) {
    testdbobj <- expect_no_error(make_testdbobj(db))
    expect_no_error(edb_create_tables(testdbobj$con))

    # Create first item with specific data
    item_data_1 <- list(
      item = list(
        title = "GenBank Database",
        container_title = "Nucleic Acids Research",
        issued = as.Date("2020-01-15")
      ),
      author = list(
        list(family = "Smith", given = "John")
      )
    )

    # Insert first item (uses insert_new_object which calls private insert_new_item)
    item_id_1 <- expect_no_error(testdbobj$insert_new_object("item", item_data_1, stage = 0))

    # Get the citation_key that was generated
    # retrieve() returns a list, extract first element
    item_1 <- testdbobj$retrieve("item", item_id_1)[[1]]
    citekey_1 <- item_1$citation_key

    # Verify the citekey exists
    # Need to check out a connection from the pool for .citekey_exists_in_db()
    checked_out_con <- pool::poolCheckout(testdbobj$con)
    tryCatch({
      expect_true(.citekey_exists_in_db(checked_out_con, citekey_1))
    }, finally = {
      pool::poolReturn(checked_out_con)
    })

    # Create second item with identical surname/year/title (would generate same citekey)
    item_data_2 <- list(
      item = list(
        title = "GenBank Database",  # Same title
        container_title = "Different Journal",
        issued = as.Date("2020-06-20")  # Same year
      ),
      author = list(
        list(family = "Smith", given = "Jane")  # Same surname
      )
    )

    # Insert second item - should get different citekey due to collision detection
    item_id_2 <- expect_no_error(testdbobj$insert_new_object("item", item_data_2, stage = 0))

    # Get the citation_key for second item
    item_2 <- testdbobj$retrieve("item", item_id_2)[[1]]
    citekey_2 <- item_2$citation_key

    # Verify both citekeys exist but are different
    checked_out_con <- pool::poolCheckout(testdbobj$con)
    tryCatch({
      expect_true(.citekey_exists_in_db(checked_out_con, citekey_2))
    }, finally = {
      pool::poolReturn(checked_out_con)
    })
    expect_false(citekey_1 == citekey_2)

    # Verify both items are in database with different IDs
    expect_false(item_id_1 == item_id_2)
  }
})

# NULL/NA edge case tests for finding functions ------------------------------

test_that(".find_item_by_identifier handles NULL and NA parameters", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # All NULL - should return empty result
    found_all_null <- .find_item_by_identifier(testcon, doi=NULL, pmid=NULL, pmcid=NULL)
    expect_equal(nrow(found_all_null), 0)

    # All NA - should return empty result
    found_all_na <- .find_item_by_identifier(testcon, doi=NA, pmid=NA, pmcid=NA)
    expect_equal(nrow(found_all_na), 0)

    # Mixed NULL and NA - should return empty result
    found_mixed_1 <- .find_item_by_identifier(testcon, doi=NULL, pmid=NA, pmcid=NULL)
    expect_equal(nrow(found_mixed_1), 0)

    found_mixed_2 <- .find_item_by_identifier(testcon, doi=NA, pmid=NULL, pmcid=NA)
    expect_equal(nrow(found_mixed_2), 0)

    # Empty strings - should return empty result (no match)
    found_empty <- .find_item_by_identifier(testcon, doi="", pmid="", pmcid="")
    expect_equal(nrow(found_empty), 0)

    # One valid, rest NULL - should work if there's a match
    proto_new_item <- list(
      title = "Test Article",
      doi = "10.1234/test",
      pmid = "12345678",
      pmcid = "PMC1234567"
    )
    new_item <- .new_object(testcon, "item", proto_new_item)
    new_item_id <- .insert_one(testcon, new_item)

    found_doi_only <- .find_item_by_identifier(testcon, doi="10.1234/test", pmid=NULL, pmcid=NULL)
    expect_true(nrow(found_doi_only) > 0)
    expect_equal(found_doi_only$item_id[1], new_item_id)

    found_pmid_only <- .find_item_by_identifier(testcon, doi=NULL, pmid="12345678", pmcid=NULL)
    expect_true(nrow(found_pmid_only) > 0)
    expect_equal(found_pmid_only$item_id[1], new_item_id)

    found_pmcid_only <- .find_item_by_identifier(testcon, doi=NULL, pmid=NULL, pmcid="PMC1234567")
    expect_true(nrow(found_pmcid_only) > 0)
    expect_equal(found_pmcid_only$item_id[1], new_item_id)

    # One valid, rest NA - should also work
    found_doi_na <- .find_item_by_identifier(testcon, doi="10.1234/test", pmid=NA, pmcid=NA)
    expect_true(nrow(found_doi_na) > 0)
    expect_equal(found_doi_na$item_id[1], new_item_id)
  }
})

test_that(".find_item_by_year_volume_page handles incomplete data", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Insert a test item with year/volume/page
    proto_new_item <- list(
      title = "Test Article",
      volume = "42",
      page_first = "123",
      issued = as.Date("2020-01-01")
    )
    new_item <- .new_object(testcon, "item", proto_new_item)
    new_item_id <- .insert_one(testcon, new_item)

    # Missing year (NULL) - should return empty result (requires all three)
    found_no_year <- .find_item_by_year_volume_page(testcon, year=NULL, volume="42", first_page="123")
    expect_equal(nrow(found_no_year), 0)

    # Missing year (NA) - should return empty result
    found_na_year <- .find_item_by_year_volume_page(testcon, year=NA, volume="42", first_page="123")
    expect_equal(nrow(found_na_year), 0)

    # Missing volume (NULL) - should return empty result
    found_no_volume <- .find_item_by_year_volume_page(testcon, year="2020", volume=NULL, first_page="123")
    expect_equal(nrow(found_no_volume), 0)

    # Missing volume (NA) - should return empty result
    found_na_volume <- .find_item_by_year_volume_page(testcon, year="2020", volume=NA, first_page="123")
    expect_equal(nrow(found_na_volume), 0)

    # Missing first_page (NULL) - should return empty result
    found_no_page <- .find_item_by_year_volume_page(testcon, year="2020", volume="42", first_page=NULL)
    expect_equal(nrow(found_no_page), 0)

    # Missing first_page (NA) - should return empty result
    found_na_page <- .find_item_by_year_volume_page(testcon, year="2020", volume="42", first_page=NA)
    expect_equal(nrow(found_na_page), 0)

    # All NULL - should return empty result
    found_all_null <- .find_item_by_year_volume_page(testcon, year=NULL, volume=NULL, first_page=NULL)
    expect_equal(nrow(found_all_null), 0)

    # All NA - should return empty result
    found_all_na <- .find_item_by_year_volume_page(testcon, year=NA, volume=NA, first_page=NA)
    expect_equal(nrow(found_all_na), 0)

    # Mixed NULL and NA - should return empty result
    found_mixed <- .find_item_by_year_volume_page(testcon, year=NULL, volume=NA, first_page=NULL)
    expect_equal(nrow(found_mixed), 0)

    # All three valid - should find the item
    found_valid <- .find_item_by_year_volume_page(testcon, year="2020", volume="42", first_page="123")
    expect_true(nrow(found_valid) > 0)
    expect_equal(found_valid$item_id[1], new_item_id)
  }
})

test_that(".find_item_by_person handles NULL and NA person_id", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # NULL person_id - should return empty result
    found_null <- .find_item_by_person(testcon, person_id=NULL)
    expect_equal(nrow(found_null), 0)

    # NA person_id - should return empty result
    found_na <- .find_item_by_person(testcon, person_id=NA)
    expect_equal(nrow(found_na), 0)

    # Non-existent person_id (valid UUID format but not in DB) - should return empty result
    found_nonexistent <- .find_item_by_person(testcon, person_id="00000000-0000-0000-0000-000000000000")
    expect_equal(nrow(found_nonexistent), 0)

    # Create item, person, and link them to verify valid search still works
    proto_new_item <- list(
      title = "Test Article",
      container_title = "Test Journal"
    )
    new_item <- .new_object(testcon, "item", proto_new_item)
    new_item_id <- .insert_one(testcon, new_item)

    proto_personlist <- list(
      item_id = new_item_id,
      personlist_type = "author"
    )
    new_personlist <- .new_object(testcon, "personlist", proto_personlist)
    personlist_id <- .insert_one(testcon, new_personlist)

    proto_new_person <- list(
      surnames = "Smith",
      primary_given_names = "John"
    )
    new_person <- .new_object(testcon, "person", proto_new_person)
    new_person_id <- .insert_one(testcon, new_person)

    proto_item_person <- list(
      personlist_id = personlist_id,
      person_id = new_person_id,
      family = "Smith",
      given = "John"
    )
    new_item_person <- .new_object(testcon, "item_person", proto_item_person)
    .insert_one(testcon, new_item_person)

    # Valid person_id - should find the item
    found_valid <- .find_item_by_person(testcon, person_id=new_person_id)
    expect_true(nrow(found_valid) > 0)
    expect_equal(found_valid$item_id[1], new_item_id)
  }
})
