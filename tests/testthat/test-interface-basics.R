
test_that(".new_object has minimal function", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    proto_new_person <- list(primary_given_names="John",
                             other_given_names="D.",
                             surnames="Doe")
    new_person <- expect_no_condition(.new_object(testcon, "person", proto_new_person))

    for (column in table_columns(testcon, "persons")) {
      expect_true(column %in% names(new_person))
    }

    proto_new_item <- list(title="The best paper ever written",
                           container_title="Journal of the Best Papers",
                           container_title_short="J Best Papers",
                           volume=1,
                           issue=1,
                           page="1-10"
    )
    new_item <- expect_no_condition(.new_object(testcon, "item", proto_new_item))

    for (column in table_columns(testcon, "items")) {
      expect_true(column %in% names(new_item))
    }
  }
})

test_that("ErudicionDB$new_object has minimal function", {
  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    proto_new_person <- list(primary_given_names="John",
                             other_given_names="D.",
                             surnames="Doe")
    new_person <- expect_no_condition(testdbobj$new_object("person", proto_new_person))

    for (column in table_columns(testdbobj$con, "persons")) {
      expect_true(column %in% names(new_person))
    }

    proto_new_item <- list(title="The best paper ever written",
                           container_title="Journal of the Best Papers",
                           container_title_short="J Best Papers",
                           volume=1,
                           issue=1,
                           page="1-10"
    )
    new_item <- expect_no_condition(testdbobj$new_object("item", proto_new_item))

    for (column in table_columns(testdbobj$con, "items")) {
      expect_true(column %in% names(new_item))
    }
  }
})


test_that(".new_object throws error for unknown columns", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    proto_new_person <- list(primary_given_names="John",
                             other_given_names="D.",
                             surnames="Doe",
                             superpower="flight") # superpower is not a column
    new_person <- expect_error(.new_object(testcon, "person", proto_new_person))

    proto_new_item <- list(title="The best paper ever written",
                           container_title="Journal of the Best Papers",
                           container_title_short="J Best Papers",
                           volume=1,
                           issue=1,
                           page="1-10",
                           price="$10" # price is not a valid column
    )
    new_item <- expect_error(.new_object(testcon, "item", proto_new_item))

  }
})

test_that("ErudicionDB$new_object throws error for unknown columns", {
  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    proto_new_person <- list(primary_given_names="John",
                             other_given_names="D.",
                             surnames="Doe",
                             superpower="flight") # superpower is not a column
    new_person <- expect_error(testdbobj$new_object("person", proto_new_person))

    proto_new_item <- list(title="The best paper ever written",
                           container_title="Journal of the Best Papers",
                           container_title_short="J Best Papers",
                           volume=1,
                           issue=1,
                           page="1-10",
                           price="$10" # price is not a valid column
    )
    new_item <- expect_error(testdbobj$new_object("item", proto_new_item))

  }
})

test_that("object can be inserted and retrieved from database", {
  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    proto_new_person_1 <- list(primary_given_names = "Tammy",
                               other_given_names = "Sue",
                               surnames = "Sweetie")
    new_person_1 <- expect_no_condition(testdbobj$new_object("person", proto_new_person_1))

    new_person_1_id <- expect_no_condition(testdbobj$insert_object(new_person_1))
    retrieved_people <- expect_no_condition(testdbobj$retrieve("person", new_person_1_id))

    expect_true(length(retrieved_people) == 1)
    retrieved_person_1 <- retrieved_people[[1]]

    expect_equal(new_person_1$primary_given_names, retrieved_person_1$primary_given_names)
    expect_equal(new_person_1$other_given_names, retrieved_person_1$other_given_names)
    expect_equal(new_person_1$surnames, retrieved_person_1$surnames)

    # can we pull a specific revision?
    retrieved_people_2 <- expect_no_condition(testdbobj$retrieve("person", new_person_1_id, revision = 1))

    expect_true(length(retrieved_people_2) == 1)
    retrieved_person_2 <- retrieved_people_2[[1]]
    expect_equal(new_person_1$primary_given_names, retrieved_person_2$primary_given_names)
  }
})

test_that("ErudicionDB interface can insert and retrieve object from database", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    proto_new_person_1 <- list(primary_given_names = "Tammy",
                               other_given_names = "Sue",
                               surnames = "Sweetie")
    new_person_1 <- expect_no_condition(.new_object(testcon, "person", proto_new_person_1,
                                                    augment_function = .augment_person))

    new_person_1_id <- expect_no_condition(.insert_one(testcon, new_person_1))
    retrieved_people <- expect_no_condition(.retrieve(testcon, "person", new_person_1_id))

    expect_true(length(retrieved_people) == 1)
    retrieved_person_1 <- retrieved_people[[1]]

    expect_equal(new_person_1$primary_given_names, retrieved_person_1$primary_given_names)
    expect_equal(new_person_1$other_given_names, retrieved_person_1$other_given_names)
    expect_equal(new_person_1$surnames, retrieved_person_1$surnames)

    # can we pull a specific revision?
    retrieved_people_2 <- expect_no_condition(.retrieve(testcon, "person", new_person_1_id,
                                                        revision = 1))

    expect_true(length(retrieved_people_2) == 1)
    retrieved_person_2 <- retrieved_people_2[[1]]
    expect_equal(new_person_1$primary_given_names, retrieved_person_2$primary_given_names)
  }
})

test_that(".erudicion_db object has basic functionality", {
  for (db in supported_databases()) {
    this_dbobj <- expect_no_condition(make_testdbobj(db))
    expect_true(inherits(this_dbobj$con, "Pool"))
    expect_no_error(edb_create_tables(this_dbobj$con))
    items_tbl <- expect_no_error(this_dbobj$tbl("items"))
    expect_true(inherits(items_tbl, "tbl"))
    expect_equal(0, this_dbobj$tbl("items") |> dplyr::count() |> dplyr::pull(n))
  }
})

# this is a pretty weak test - only verifies that the functions exist
# and don't immediately explode
test_that("dbobj$add_augmentor and dbobj$add_validator work", {
  for (db in supported_databases()) {
    this_dbobj <- expect_no_condition(make_testdbobj(db))
    test_augmentor <- function(x) {x}
    expect_no_error(this_dbobj$add_augmentor("test", test_augmentor))
    test_validator <- function(x) {TRUE}
    expect_no_error(this_dbobj$add_validator("test", test_validator))
  }
})


# Tests for ErudicionDB$disconnect() ------------------------------------------

test_that("ErudicionDB$disconnect closes connection pool", {
  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    # Verify pool is valid before disconnect
    expect_true(inherits(testdbobj$con, "Pool"))
    expect_true(DBI::dbIsValid(testdbobj$con))

    # Disconnect should not error
    expect_no_error(testdbobj$disconnect())

    # After disconnect, pool should be NULL
    expect_null(testdbobj$con)
  }
})

test_that("ErudicionDB$disconnect is idempotent (multiple calls are safe)", {
  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    # First disconnect
    expect_no_error(testdbobj$disconnect())
    expect_null(testdbobj$con)

    # Second disconnect should not error (pool already NULL)
    expect_no_error(testdbobj$disconnect())
    expect_null(testdbobj$con)

    # Third disconnect should also not error
    expect_no_error(testdbobj$disconnect())
    expect_null(testdbobj$con)
  }
})

test_that("ErudicionDB can reconnect after disconnect", {
  for (db in supported_databases()) {
    # Create initial connection
    dbargs <- if (db == "sqlite") {
      list(drv = RSQLite::SQLite(), dbname = ":memory:")
    } else {
      list(drv = duckdb::duckdb(), dbdir = ":memory:")
    }

    testdbobj <- ErudicionDB$new(dbargs)
    expect_no_error(edb_create_tables(testdbobj$con))

    # Verify initial connection works
    expect_true(DBI::dbIsValid(testdbobj$con))

    # Disconnect
    testdbobj$disconnect()
    expect_null(testdbobj$con)

    # Reconnect using establish_connection
    expect_no_error(testdbobj$establish_connection(dbargs))

    # Verify pool is valid again
    expect_true(inherits(testdbobj$con, "Pool"))
    expect_true(DBI::dbIsValid(testdbobj$con))

    # Clean up
    testdbobj$disconnect()
  }
})

test_that("ErudicionDB is functional after reconnect", {
  for (db in supported_databases()) {
    # Create initial connection
    dbargs <- if (db == "sqlite") {
      list(drv = RSQLite::SQLite(), dbname = ":memory:")
    } else {
      list(drv = duckdb::duckdb(), dbdir = ":memory:")
    }

    testdbobj <- ErudicionDB$new(dbargs)
    expect_no_error(edb_create_tables(testdbobj$con))

    # Insert a person before disconnect
    proto_person_1 <- list(
      primary_given_names = "Jane",
      surnames = "Smith"
    )
    person_id_1 <- expect_no_error(testdbobj$insert_new_object("person", proto_person_1))

    # Verify person exists
    retrieved_1 <- testdbobj$retrieve("person", person_id_1)
    expect_equal(length(retrieved_1), 1)
    expect_equal(retrieved_1[[1]]$surnames, "Smith")

    # Disconnect and reconnect
    testdbobj$disconnect()
    expect_null(testdbobj$con)

    # Reconnect - need to recreate tables since memory DB is lost
    testdbobj$establish_connection(dbargs)
    expect_no_error(edb_create_tables(testdbobj$con))

    # Insert another person after reconnect to verify functionality
    proto_person_2 <- list(
      primary_given_names = "John",
      surnames = "Doe"
    )
    person_id_2 <- expect_no_error(testdbobj$insert_new_object("person", proto_person_2))

    # Verify new person exists and database operations work
    retrieved_2 <- testdbobj$retrieve("person", person_id_2)
    expect_equal(length(retrieved_2), 1)
    expect_equal(retrieved_2[[1]]$surnames, "Doe")

    # Verify tbl() method works after reconnect
    persons_count <- testdbobj$tbl("persons") |> dplyr::count() |> dplyr::pull(n)
    expect_equal(persons_count, 1)  # Only the new person (memory DB was reset)

    # Clean up
    testdbobj$disconnect()
  }
})

test_that("ErudicionDB$disconnect prevents operations on closed connection", {
  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    # Disconnect
    testdbobj$disconnect()
    expect_null(testdbobj$con)

    # Attempting to use tbl() on NULL pool should error
    expect_error(testdbobj$tbl("items"))

    # Attempting database operations should also error
    proto_person <- list(
      primary_given_names = "Test",
      surnames = "Person"
    )
    expect_error(testdbobj$insert_new_object("person", proto_person))
  }
})


# Tests for ErudicionDB$insert_object() ---------------------------------------

test_that("ErudicionDB$insert_object inserts pre-validated person objects", {
  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    # Create a person object using new_object (validates and augments)
    proto_person <- list(
      primary_given_names = "Alice",
      other_given_names = "Marie",
      surnames = "Johnson"
    )
    person_obj <- expect_no_error(testdbobj$new_object("person", proto_person))

    # Insert the pre-validated object
    person_id <- expect_no_error(testdbobj$insert_object(person_obj))
    expect_true(!is.null(person_id))

    # Verify person appears in database correctly
    retrieved <- testdbobj$retrieve("person", person_id)
    expect_equal(length(retrieved), 1)
    retrieved_person <- retrieved[[1]]

    expect_equal(retrieved_person$primary_given_names, "Alice")
    expect_equal(retrieved_person$other_given_names, "Marie")
    expect_equal(retrieved_person$surnames, "Johnson")
    expect_equal(retrieved_person$person_id, person_id)

    # Verify augmented fields exist (added by .augment_person)
    expect_true("ascii_given_names" %in% names(retrieved_person))
    expect_true("ascii_surnames" %in% names(retrieved_person))
    expect_true("short_referent" %in% names(retrieved_person))
    expect_equal(retrieved_person$short_referent, "Alice")
  }
})

test_that("ErudicionDB$insert_object inserts pre-validated item objects", {
  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    # Create an item object using new_object
    proto_item <- list(
      title = "A Study of Database Design",
      container_title = "Journal of Computing",
      volume = 42,
      issue = 3,
      page = "100-120"
    )
    item_obj <- expect_no_error(testdbobj$new_object("item", proto_item))

    # Insert the pre-validated object
    item_id <- expect_no_error(testdbobj$insert_object(item_obj))
    expect_true(!is.null(item_id))

    # Verify item appears in database correctly
    retrieved <- testdbobj$retrieve("item", item_id)
    expect_equal(length(retrieved), 1)
    retrieved_item <- retrieved[[1]]

    expect_equal(retrieved_item$title, "A Study of Database Design")
    expect_equal(retrieved_item$container_title, "Journal of Computing")
    expect_equal(retrieved_item$volume, "42")  # Stored/retrieved as string
    expect_equal(retrieved_item$issue, "3")     # Stored/retrieved as string
    expect_equal(retrieved_item$page, "100-120")
    expect_equal(retrieved_item$item_id, item_id)
  }
})

test_that("ErudicionDB$insert_object inserts pre-validated person_identifier objects", {
  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    # Create a person_identifier object using new_object
    proto_identifier <- list(
      person_id = uuid::UUIDgenerate(),
      id_type = "orcid",
      id_value = "0000-0001-2345-6789"
    )
    identifier_obj <- expect_no_error(testdbobj$new_object("person_identifier", proto_identifier))

    # Insert the pre-validated object
    identifier_id <- expect_no_error(testdbobj$insert_object(identifier_obj))
    expect_true(!is.null(identifier_id))

    # Verify person_identifier appears in database correctly
    retrieved <- testdbobj$retrieve("person_identifier", identifier_id)
    expect_equal(length(retrieved), 1)
    retrieved_identifier <- retrieved[[1]]

    expect_equal(retrieved_identifier$id_type, "orcid")
    expect_equal(retrieved_identifier$id_value, "0000-0001-2345-6789")
    expect_equal(retrieved_identifier$person_identifier_id, identifier_id)
  }
})

test_that("ErudicionDB$insert_object bypasses validation (by design)", {
  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    # Demonstrate that insert_object bypasses validation by creating
    # an object manually without going through new_object()
    # This person object lacks fields that would normally be added by augmentors

    # Get table columns to know what fields are required
    checked_out_con <- pool::poolCheckout(testdbobj$con)
    person_cols <- table_columns(checked_out_con, "persons")
    pool::poolReturn(checked_out_con)

    # Create minimal person object with only required database fields
    manual_person <- list(
      person_id = uuid::UUIDgenerate(),
      surnames = "ManualEntry",
      primary_given_names = "Test",
      revision = 1,
      stage = 0,
      created = Sys.time(),
      object_type = "person"
    )

    # This bypasses the augmentor that would add initials, ascii variants, etc.
    # insert_object accepts it without validation/augmentation
    person_id <- expect_no_error(testdbobj$insert_object(manual_person))
    expect_equal(person_id, manual_person$person_id)

    # Verify object was inserted even without augmentation
    retrieved <- testdbobj$retrieve("person", person_id)
    expect_equal(length(retrieved), 1)
    expect_equal(retrieved[[1]]$surnames, "ManualEntry")

    # Note: Fields added by augmentor (like initials) will be NULL/NA
    # because insert_object bypassed the augmentation step
  }
})

test_that("ErudicionDB$insert_object works with multiple object types", {
  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    # Insert person
    person_obj <- testdbobj$new_object("person", list(
      surnames = "Williams",
      primary_given_names = "Emma"
    ))
    person_id <- expect_no_error(testdbobj$insert_object(person_obj))

    # Insert item
    item_obj <- testdbobj$new_object("item", list(
      title = "Test Article",
      container_title = "Test Journal"
    ))
    item_id <- expect_no_error(testdbobj$insert_object(item_obj))

    # Insert personlist
    personlist_obj <- testdbobj$new_object("personlist", list(
      item_id = item_id,
      personlist_type = "author"
    ))
    personlist_id <- expect_no_error(testdbobj$insert_object(personlist_obj))

    # Insert item_person (linking person to personlist)
    item_person_obj <- testdbobj$new_object("item_person", list(
      personlist_id = personlist_id,
      person_id = person_id,
      family = "Williams",
      given = "Emma"
    ))
    item_person_id <- expect_no_error(testdbobj$insert_object(item_person_obj))

    # Verify all objects were inserted
    expect_true(!is.null(person_id))
    expect_true(!is.null(item_id))
    expect_true(!is.null(personlist_id))
    expect_true(!is.null(item_person_id))

    # Verify retrieval works for all
    expect_equal(length(testdbobj$retrieve("person", person_id)), 1)
    expect_equal(length(testdbobj$retrieve("item", item_id)), 1)
    expect_equal(length(testdbobj$retrieve("personlist", personlist_id)), 1)
    expect_equal(length(testdbobj$retrieve("item_person", item_person_id)), 1)
  }
})

test_that("ErudicionDB$insert_object returns correct object ID", {
  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    # Insert multiple persons and verify IDs are unique
    person1_obj <- testdbobj$new_object("person", list(surnames = "Anderson"))
    person2_obj <- testdbobj$new_object("person", list(surnames = "Baker"))
    person3_obj <- testdbobj$new_object("person", list(surnames = "Carter"))

    person1_id <- testdbobj$insert_object(person1_obj)
    person2_id <- testdbobj$insert_object(person2_obj)
    person3_id <- testdbobj$insert_object(person3_obj)

    # All IDs should be unique
    expect_false(person1_id == person2_id)
    expect_false(person2_id == person3_id)
    expect_false(person1_id == person3_id)

    # Each ID should retrieve the correct person
    expect_equal(testdbobj$retrieve("person", person1_id)[[1]]$surnames, "Anderson")
    expect_equal(testdbobj$retrieve("person", person2_id)[[1]]$surnames, "Baker")
    expect_equal(testdbobj$retrieve("person", person3_id)[[1]]$surnames, "Carter")
  }
})
