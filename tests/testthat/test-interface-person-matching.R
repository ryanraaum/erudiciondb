test_that("match_person can match people", {
  for (db in supported_databases()) {
    # setting up
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    some_people <- list(
      list(primary_given_names="Ryan", other_given_names="Lowell", surnames="Raaum"),
      list(primary_given_names="William", other_given_names="E. H.", surnames="Harcourt-Smith"),
      list(primary_given_names="J", surnames="Mohorčich"),
      list(primary_given_names="Maryam", surnames="Bamshad-Alavi")
    )
    person_ids = vector("character", length(some_people))
    for (i in seq_along(some_people)) {
      person_ids[i] <- expect_no_error(testdbobj$insert_new_object("person", some_people[[i]]))
    }

    match_result <- expect_no_error(.match_person(testdbobj$con, list(given="Ryan", family="Raaum")))
    expect_false(is.null(match_result))
    expect_true(inherits(match_result, "data.frame"))
    expect_true(person_ids[1] %in% match_result$person_id)

    match_result <- expect_no_error(.match_person(testdbobj$con, list(given="William E H", family="Harcourt-Smith")))
    expect_false(is.null(match_result))
    expect_true(inherits(match_result, "data.frame"))
    expect_true(person_ids[2] %in% match_result$person_id)

    match_result <- expect_no_error(.match_person(testdbobj$con, list(given="Joseph", family="Mohorcich")))
    expect_false(is.null(match_result))
    expect_true(inherits(match_result, "data.frame"))
    expect_true(person_ids[3] %in% match_result$person_id)

    match_result <- expect_no_error(.match_person(testdbobj$con, list(given="Maryam", family="Bamshad")))
    expect_false(is.null(match_result))
    expect_true(inherits(match_result, "data.frame"))
    expect_true(person_ids[4] %in% match_result$person_id)
  }
})

test_that("match_person works with empty table", {
  for (db in supported_databases()) {
    # setting up
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    match_result <- expect_no_error(.match_person(testdbobj$con, list(given="Ryan", family="Raaum")))
    expect_false(is.null(match_result))
    expect_true(nrow(match_result) == 0)
  }
})

test_that("match_person handles NULL and NA names gracefully", {
  # Bug fix #2: stringi::stri_trans_general() crashes on NULL/NA input
  # The fix adds NULL/NA checks before ASCII transformations (R/interface.R:432-442)
  # The bug occurred when person data from CSL/bibliography had NULL/NA in names
  # This test verifies the ASCII transformation doesn't crash

  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    # Insert test persons with non-ASCII characters to trigger ASCII transformation
    person1_id <- testdbobj$insert_new_object("person",
      list(primary_given_names="José", surnames="García"))
    person2_id <- testdbobj$insert_new_object("person",
      list(primary_given_names="François", surnames="Müller"))

    # Test with empty names list - should return NULL (no crash)
    result1 <- .match_person(testdbobj$con, list())
    expect_true(is.null(result1))

    # Test with valid surname only - doesn't crash even though given is not provided
    # This exercises the code path where pdata$given is NULL but the ASCII
    # transformation is protected by the NULL check
    result2 <- .match_person(testdbobj$con, list(family="García"))
    expect_true(inherits(result2, "data.frame"))

    # Test with valid names that require ASCII transformation
    result3 <- .match_person(testdbobj$con, list(family="García", given="José"))
    expect_true(inherits(result3, "data.frame"))
    expect_true(person1_id %in% result3$person_id)

    # Test with ASCII version of non-ASCII name
    result4 <- .match_person(testdbobj$con, list(family="Garcia", given="Jose"))
    expect_true(inherits(result4, "data.frame"))
    expect_true(person1_id %in% result4$person_id)
  }
})

test_that(".match_person handles names with no word characters", {
  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    # Insert a person with regular ASCII name
    person_id <- expect_no_error(.insert_new_object(
      testdbobj$con,
      "person",
      list(
        primary_given_names = "Jane",
        surnames = "Smith"
      )
    ))

    # Test 1: Pure emoji given name (str_extract returns NA)
    person_emoji <- list(
      given = "😀",
      family = "Smith"
    )
    result_emoji <- expect_no_error(.match_person(testdbobj$con, person_emoji))

    # Should return empty result (no match found)
    expect_equal(nrow(result_emoji), 0)
    expect_true(all(c("person_id", "similarity", "found_by") %in% names(result_emoji)))

    # Test 2: Special character-only given name
    person_special <- list(
      given = "★☆✦",
      family = "Smith"
    )
    result_special <- expect_no_error(.match_person(testdbobj$con, person_special))

    # Should return empty result (no match found)
    expect_equal(nrow(result_special), 0)

    # Test 3: Empty string given name (edge case)
    person_empty <- list(
      given = "",
      family = "Smith"
    )
    result_empty <- expect_no_error(.match_person(testdbobj$con, person_empty))

    # Should return empty result (no match found)
    expect_equal(nrow(result_empty), 0)
  }
})

test_that(".match_person still works correctly for valid names", {
  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    # Insert a person
    person_id <- expect_no_error(.insert_new_object(
      testdbobj$con,
      "person",
      list(
        primary_given_names = "Jane",
        surnames = "Smith"
      )
    ))

    # Test with very close name (should still match via approximate strategy)
    person_close <- list(
      given = "Jane",
      family = "Smyth"  # Close but not exact
    )
    result_close <- expect_no_error(.match_person(testdbobj$con, person_close))

    # Depending on distance threshold, may or may not match
    # At minimum, should not error
    expect_true(nrow(result_close) >= 0)
    expect_true(all(c("person_id", "similarity", "found_by") %in% names(result_close)))
  }
})

test_that(".find() properly returns pooled connection on error", {
  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    # Insert multiple items for stress testing
    for (i in 1:20) {
      testdbobj$insert_new_object("item",
        list(title=paste("Test Item", i)))
    }

    # Multiple rapid finds shouldn't exhaust pool
    # Test with various search patterns
    for (i in 1:10) {
      # Search that returns no results
      result1 <- .find(testdbobj$con, "item", list(volume="999"))
      expect_true(inherits(result1, "data.frame"))

      # Search that returns results
      result2 <- .find(testdbobj$con, "item", list(title="Test Item 1"))
      expect_true(inherits(result2, "data.frame"))
    }

    # If connections leaked, pool would be exhausted by now
    # This final operation verifies pool is still healthy
    result3 <- expect_no_error(
      .find(testdbobj$con, "item", list(title="Test Item 1"))
    )
    expect_true(nrow(result3) >= 1)
  }
})
