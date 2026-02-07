
test_that("create_tables creates sqlite tables", {
  testcon <- make_testcon("sqlite")
  expect_no_error(edb_create_tables(testcon))
})

test_that("create_tables creates duckdb tables", {
  testcon <- make_testcon("duckdb")
  expect_no_error(edb_create_tables(testcon))
})

test_that("create_tables creates individual tables", {
  testcon <- make_testcon("duckdb")
  for (table in names(create_table_functions)) {
    expect_no_error(edb_create_tables(testcon, table))
  }
})

test_that("create_tables warns about unknown tables", {
  testcon <- make_testcon("duckdb")
  expect_warning(edb_create_tables(testcon, "unknown"))
})

# Individual table creation tests --------------------------------------------

test_that(".create_persons_table creates persons table correctly", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)

    # Create the table
    expect_no_error(.create_persons_table(testcon))

    # Verify table exists
    expect_true(DBI::dbExistsTable(testcon, "persons"))

    # Verify key columns exist
    columns <- DBI::dbListFields(testcon, "persons")
    expect_true("person_id" %in% columns)
    expect_true("surnames" %in% columns)
    expect_true("primary_given_names" %in% columns)
    expect_true("ascii_surnames" %in% columns)

    # Verify revision tracking columns exist
    expect_true("revision" %in% columns)
    expect_true("stage" %in% columns)
    expect_true("created" %in% columns)
  }
})

test_that(".create_person_roles_table creates person_roles table correctly", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)

    expect_no_error(.create_person_roles_table(testcon))
    expect_true(DBI::dbExistsTable(testcon, "person_roles"))

    columns <- DBI::dbListFields(testcon, "person_roles")
    expect_true("person_role_id" %in% columns)
    expect_true("person_id" %in% columns)
    expect_true("position" %in% columns)
    expect_true("organization" %in% columns)

    # Revision tracking columns
    expect_true("revision" %in% columns)
    expect_true("stage" %in% columns)
    expect_true("created" %in% columns)
  }
})

test_that(".create_item_persons_table creates item_persons table correctly", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)

    expect_no_error(.create_item_persons_table(testcon))
    expect_true(DBI::dbExistsTable(testcon, "item_persons"))

    columns <- DBI::dbListFields(testcon, "item_persons")
    expect_true("item_person_id" %in% columns)
    expect_true("personlist_id" %in% columns)
    expect_true("person_id" %in% columns)
    expect_true("family" %in% columns)
    expect_true("given" %in% columns)

    # Revision tracking columns
    expect_true("revision" %in% columns)
    expect_true("stage" %in% columns)
    expect_true("created" %in% columns)
  }
})

test_that(".create_personlists_table creates personlists table correctly", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)

    expect_no_error(.create_personlists_table(testcon))
    expect_true(DBI::dbExistsTable(testcon, "personlists"))

    columns <- DBI::dbListFields(testcon, "personlists")
    expect_true("personlist_id" %in% columns)
    expect_true("item_id" %in% columns)
    expect_true("personlist_type" %in% columns)

    # Revision tracking columns
    expect_true("revision" %in% columns)
    expect_true("stage" %in% columns)
    expect_true("created" %in% columns)
  }
})

test_that(".create_affiliation_references_table creates affiliation_references table correctly", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)

    expect_no_error(.create_affiliation_references_table(testcon))
    expect_true(DBI::dbExistsTable(testcon, "affiliation_references"))

    columns <- DBI::dbListFields(testcon, "affiliation_references")
    expect_true("affiliation_reference_id" %in% columns)
    expect_true("item_person_id" %in% columns)
    expect_true("position" %in% columns)
    expect_true("affiliation" %in% columns)

    # Revision tracking columns
    expect_true("revision" %in% columns)
    expect_true("stage" %in% columns)
    expect_true("created" %in% columns)
  }
})

test_that(".create_items_table creates items table correctly", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)

    expect_no_error(.create_items_table(testcon))
    expect_true(DBI::dbExistsTable(testcon, "items"))

    columns <- DBI::dbListFields(testcon, "items")
    expect_true("item_id" %in% columns)
    expect_true("title" %in% columns)
    expect_true("doi" %in% columns)
    expect_true("pmid" %in% columns)
    expect_true("pmcid" %in% columns)
    expect_true("container_title" %in% columns)
    expect_true("volume" %in% columns)
    expect_true("issued" %in% columns)

    # Revision tracking columns
    expect_true("revision" %in% columns)
    expect_true("stage" %in% columns)
    expect_true("created" %in% columns)
  }
})

test_that(".create_issues_table creates issues table correctly", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)

    expect_no_error(.create_issues_table(testcon))
    expect_true(DBI::dbExistsTable(testcon, "issues"))

    columns <- DBI::dbListFields(testcon, "issues")
    expect_true("issue_id" %in% columns)
    expect_true("object_type" %in% columns)
    expect_true("object_id" %in% columns)
    expect_true("status" %in% columns)
    expect_true("description" %in% columns)

    # Revision tracking columns
    expect_true("revision" %in% columns)
    expect_true("stage" %in% columns)
    expect_true("created" %in% columns)
  }
})

test_that(".create_person_identifiers_table creates person_identifiers table correctly", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)

    expect_no_error(.create_person_identifiers_table(testcon))
    expect_true(DBI::dbExistsTable(testcon, "person_identifiers"))

    columns <- DBI::dbListFields(testcon, "person_identifiers")
    expect_true("person_identifier_id" %in% columns)
    expect_true("person_id" %in% columns)
    expect_true("id_type" %in% columns)
    expect_true("id_value" %in% columns)

    # Revision tracking columns
    expect_true("revision" %in% columns)
    expect_true("stage" %in% columns)
    expect_true("created" %in% columns)
  }
})

test_that(".create_item_person_identifiers_table creates item_person_identifiers table correctly", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)

    expect_no_error(.create_item_person_identifiers_table(testcon))
    expect_true(DBI::dbExistsTable(testcon, "item_person_identifiers"))

    columns <- DBI::dbListFields(testcon, "item_person_identifiers")
    expect_true("item_person_identifier_id" %in% columns)
    expect_true("item_person_id" %in% columns)
    expect_true("id_type" %in% columns)
    expect_true("id_value" %in% columns)

    # Revision tracking columns
    expect_true("revision" %in% columns)
    expect_true("stage" %in% columns)
    expect_true("created" %in% columns)
  }
})

test_that("all tables can be queried after creation", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon, "all"))

    # Verify all tables are empty and queryable
    table_names <- c(
      "persons", "person_roles", "item_persons", "personlists",
      "affiliation_references", "items", "issues",
      "person_identifiers", "item_person_identifiers"
    )

    for (table_name in table_names) {
      result <- DBI::dbGetQuery(testcon, glue::glue_sql("SELECT COUNT(*) as n FROM {`table_name`}", .con = testcon))
      expect_equal(result$n, 0)
    }
  }
})

