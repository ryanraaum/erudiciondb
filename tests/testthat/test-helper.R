
test_that(".initials makes initials", {
  expect_equal(.initials("Ashley"), "A")
  expect_equal(.initials("Ashley Victoria"), "AV")
  expect_equal(.initials(c("Ashley V.")), "AV")
  expect_equal(.initials(c("A. V.")), "AV")
  expect_equal(.initials(c("A V")), "AV")
  expect_equal(.initials("Ashley Victoria Priscilla"), "AVP")
  expect_equal(.initials("Åshley Victoria"), "ÅV")
  expect_equal(.initials("Åshley Éxperion"), "ÅÉ")
  expect_equal(.initials("Åshley Éxpêrion"), "ÅÉ")
  expect_equal(.initials("Bah-Humbug"), "B-H")
})

test_that(".word_from_title extracts word(s) from title", {
  expect_equal(.word_from_title("Future Islands"), "Future")
  expect_equal(.word_from_title("The Future Islands"), "Future")
  expect_equal(.word_from_title("Future Islands", n=2), c("Future","Islands"))
  expect_equal(.word_from_title("The Future Islands", n=2), c("Future","Islands"))
})

test_that(".make_citekey works", {
  expect_equal(.make_citekey("Clark", "2015", "GenBank"), "Clark2015Genbank")
  expect_equal(.make_citekey("Clark", "2015", "The GenBank"), "Clark2015Genbank")
})

test_that(".select_surname works", {
  test_item_1 <- list(author=list(list(family="Clark")))
  test_item_2 <- list(author=list(list(literal="The International HapMap Consortium")))
  test_item_3 <- list(author=list(list(given="Jeremy")))
  expect_equal(.select_surname(test_item_1), "Clark")
  expect_equal(.select_surname(test_item_2), "The International HapMap Consortium")
  expect_true(is.na(.select_surname(test_item_3)))
})

test_that(".select_surname handles edge cases", {
  # Test with empty personlists (no author, editor, etc.)
  test_item_empty <- list(
    item = list(title="Item with no creators")
  )
  expect_true(is.na(.select_surname(test_item_empty)))

  # Test with personlist that exists but has empty array
  test_item_empty_array <- list(
    author = list()
  )
  expect_true(is.na(.select_surname(test_item_empty_array)))

  # Test with personlist where first element is NULL
  test_item_null_first <- list(
    author = list(NULL)
  )
  expect_true(is.na(.select_surname(test_item_null_first)))

  # Test with personlist where first element has no family or literal
  test_item_no_names <- list(
    author = list(list(given="John"))
  )
  expect_true(is.na(.select_surname(test_item_no_names)))

  # Verify it still works for valid cases
  test_item_valid <- list(
    author = list(list(family="Smith", given="John"))
  )
  expect_equal(.select_surname(test_item_valid), "Smith")
})

test_that(".select_year works", {
  test_item_1 <- list(item=list(issued="2025-03-10 20:21:47 UTC"))
  test_item_2 <- list(item=list(title="This is the title"))
  expect_equal(.select_year(test_item_1), 2025)
  expect_true(is.na(.select_year(test_item_2)))
})

test_that(".select_title works", {
  test_item_1 <- list(item=list(title="This is the title"))
  test_item_2 <- list(item=list(issued="2025-03-10 20:21:47 UTC"))
  expect_equal(.select_title(test_item_1), "This is the title")
  expect_true(is.na(.select_title(test_item_2)))
})

# lapply filters work on lists of lists

test_that(".lapply_filter_na works", {
  test_lofl <- list(list(a=1, b=NA),
                    list(a=2, b=NA, c=NA))
  expect_true("a" %in% names(test_lofl[[1]]))
  expect_true("a" %in% names(test_lofl[[2]]))
  expect_true("b" %in% names(test_lofl[[1]]))
  expect_true("b" %in% names(test_lofl[[2]]))
  expect_true("c" %in% names(test_lofl[[2]]))

  filtered <- expect_no_error(.lapply_filter_na(test_lofl))
  expect_true(length(filtered) == 2)
  expect_true("a" %in% names(filtered[[1]]))
  expect_true("a" %in% names(filtered[[2]]))
  expect_false("b" %in% names(filtered[[1]]))
  expect_false("b" %in% names(filtered[[2]]))
  expect_false("c" %in% names(filtered[[2]]))
})

test_that(".lapply_filter_internal works", {
  test_lofl <- list(list(a=1, this_id=1, stage=0, revision=3, created=4),
                    list(a=2, that_id=2, object_type="test_thing"))
  expect_true("a" %in% names(test_lofl[[1]]))
  expect_true("a" %in% names(test_lofl[[2]]))
  expect_true("this_id" %in% names(test_lofl[[1]]))
  expect_true("stage" %in% names(test_lofl[[1]]))
  expect_true("revision" %in% names(test_lofl[[1]]))
  expect_true("created" %in% names(test_lofl[[1]]))
  expect_true("that_id" %in% names(test_lofl[[2]]))
  expect_true("object_type" %in% names(test_lofl[[2]]))

  filtered <- expect_no_error(.lapply_filter_internal(test_lofl))
  expect_true(length(filtered) == 2)
  expect_true("a" %in% names(filtered[[1]]))
  expect_true("a" %in% names(filtered[[2]]))
  expect_false("this_id" %in% names(filtered[[1]]))
  expect_false("stage" %in% names(filtered[[1]]))
  expect_false("revision" %in% names(filtered[[1]]))
  expect_false("created" %in% names(filtered[[1]]))
  expect_false("that_id" %in% names(filtered[[2]]))
  expect_false("object_type" %in% names(filtered[[2]]))
})

test_that(".filter_internal validates keep parameter type", {
  test_list <- list(
    a = 1,
    this_id = 2,
    stage = 0,
    revision = 1
  )

  # Valid: NULL keep (default)
  result1 <- expect_no_error(.filter_internal(test_list, keep=NULL))
  expect_true("a" %in% names(result1))
  expect_false("this_id" %in% names(result1))

  # Valid: character vector keep
  result2 <- expect_no_error(.filter_internal(test_list, keep=c("stage")))
  expect_true("a" %in% names(result2))
  expect_true("stage" %in% names(result2))
  expect_false("this_id" %in% names(result2))

  # Invalid: numeric keep
  expect_error(
    .filter_internal(test_list, keep=123),
    regexp = "keep parameter must be a character vector"
  )

  # Invalid: list keep
  expect_error(
    .filter_internal(test_list, keep=list("stage")),
    regexp = "keep parameter must be a character vector"
  )

  # Invalid: logical keep
  expect_error(
    .filter_internal(test_list, keep=TRUE),
    regexp = "keep parameter must be a character vector"
  )
})



# other filters work on just lists
test_that(".filter_na works", {
  test_l <- list(a=1, b=NA)
  expect_true("a" %in% names(test_l))
  expect_true("a" %in% names(test_l))

  filtered <- expect_no_error(.filter_na(test_l))
  expect_true(length(filtered) == 1)
  expect_true("a" %in% names(filtered))
  expect_false("b" %in% names(filtered))
})

test_that(".filter_internal works", {
  test_l <- list(a=1, this_id=1, stage=0, revision=3, created=4)
  expect_true("a" %in% names(test_l))
  expect_true("this_id" %in% names(test_l))
  expect_true("stage" %in% names(test_l))
  expect_true("revision" %in% names(test_l))
  expect_true("created" %in% names(test_l))

  filtered <- expect_no_error(.filter_internal(test_l))
  expect_true(length(filtered) == 1)
  expect_true("a" %in% names(filtered))
  expect_false("this_id" %in% names(filtered))
  expect_false("stage" %in% names(filtered))
  expect_false("revision" %in% names(filtered))
  expect_false("created" %in% names(filtered))
})

# Test .update_it() helper function

test_that(".update_it returns old value when new value is NA", {
  old_obj <- list(name = "Original", value = 42)

  result <- .update_it(old_obj, "name", NA)

  expect_equal(result, "Original")
})

test_that(".update_it returns old value when new value is NULL", {
  old_obj <- list(name = "Original", value = 42)

  result <- .update_it(old_obj, "name", NULL)

  expect_equal(result, "Original")
})

test_that(".update_it returns new value when value is not NA or NULL", {
  old_obj <- list(name = "Original", value = 42)

  # Test with string
  result1 <- .update_it(old_obj, "name", "Updated")
  expect_equal(result1, "Updated")

  # Test with number
  result2 <- .update_it(old_obj, "value", 99)
  expect_equal(result2, 99)

  # Test with FALSE (should not be treated as NA)
  result3 <- .update_it(old_obj, "value", FALSE)
  expect_equal(result3, FALSE)

  # Test with 0 (should not be treated as NA)
  result4 <- .update_it(old_obj, "value", 0)
  expect_equal(result4, 0)

  # Test with empty string (should not be treated as NA)
  result5 <- .update_it(old_obj, "name", "")
  expect_equal(result5, "")
})

test_that(".update_it works within .revise_object context", {
  # Integration test to ensure the fix works in actual usage
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Create a person
    person_id <- expect_no_error(.insert_new_object(
      testcon,
      "person",
      list(
        primary_given_names = "John",
        surnames = "Doe"
      )
    ))

    # Get the person object
    person_obj <- .retrieve(testcon, "person", person_id)[[1]]

    # Revise with NULL (should keep old value via .update_it)
    expect_no_error(.update_object(testcon, person_obj, surnames = NULL))
    person_after_null <- .retrieve(testcon, "person", person_id)[[1]]
    expect_equal(person_after_null$surnames, "Doe")  # Unchanged

    # Revise with NA (should keep old value via .update_it)
    person_obj <- .retrieve(testcon, "person", person_id)[[1]]
    expect_no_error(.update_object(testcon, person_obj, surnames = NA))
    person_after_na <- .retrieve(testcon, "person", person_id)[[1]]
    expect_equal(person_after_na$surnames, "Doe")  # Still unchanged

    # Revise with actual value (should update)
    person_obj <- .retrieve(testcon, "person", person_id)[[1]]
    expect_no_error(.update_object(testcon, person_obj, surnames = "Smith"))
    person_after_update <- .retrieve(testcon, "person", person_id)[[1]]
    expect_equal(person_after_update$surnames, "Smith")  # Updated
  }
})

# Test .name_parts() helper function

test_that(".name_parts splits names correctly", {
  # Basic functionality
  expect_equal(.name_parts("John Doe"), c("John", "Doe"))
  expect_equal(.name_parts("Mary Jane Smith"), c("Mary", "Jane", "Smith"))
  expect_equal(.name_parts("Madonna"), c("Madonna"))

  # Punctuation and special characters
  expect_equal(.name_parts("Mary-Jane Smith"), c("Mary", "Jane", "Smith"))
  expect_equal(.name_parts("O'Brien"), c("O", "Brien"))
  expect_equal(.name_parts("J.R.R. Tolkien"), c("J", "R", "R", "Tolkien"))
  expect_equal(.name_parts("John  Doe"), c("John", "Doe"))  # Multiple spaces

  # Edge cases
  expect_equal(.name_parts(""), character(0))
  expect_equal(.name_parts("---"), character(0))
  expect_equal(.name_parts(" John Doe "), c("John", "Doe"))

  # International names (accented characters are word characters)
  expect_equal(.name_parts("José García"), c("José", "García"))
})

# .shortest_distance() --------------------------------------------------------

test_that(".shortest_distance calculates name similarity correctly", {
  # Perfect matches (distance = 0)
  expect_equal(.shortest_distance(c("John", "Smith"), c("John", "Smith")), 0)
  expect_equal(.shortest_distance(c("García"), c("García")), 0)

  # Single-part names (uses min of all comparisons)
  expect_equal(.shortest_distance(c("Smith"), c("John", "Smith")), 0)
  expect_equal(.shortest_distance(c("Smith"), c("John", "Smith", "Jr")), 0)

  # Equal-length multi-part names (direct comparison)
  expect_equal(.shortest_distance(c("John", "Smith"), c("John", "Smith")), 0)
  expect_equal(.shortest_distance(c("Mary", "Jane"), c("Mary", "Jane")), 0)

  # Sliding window - target longer than found (finds best alignment)
  result <- .shortest_distance(c("John", "Michael"),
                               c("John", "Michael", "Robert", "Smith"))
  expect_equal(result, 0)

  result <- .shortest_distance(c("Robert", "Smith"),
                               c("John", "Michael", "Robert", "Smith"))
  expect_equal(result, 0)

  # Sliding window - found longer than target (finds best alignment)
  result <- .shortest_distance(c("John", "Michael", "Robert", "Smith"),
                               c("Robert", "Smith"))
  expect_equal(result, 0)

  result <- .shortest_distance(c("John", "Michael", "Robert", "Smith"),
                               c("John", "Michael"))
  expect_equal(result, 0)

  # Initial filtering - single chars removed before comparison
  expect_equal(.shortest_distance(c("J", "Smith"), c("John", "Smith")), 0)

  # After filtering "A", we get "John Smith" vs "John Andrew Smith"
  # Sliding window finds best match, but "Smith" doesn't match "Andrew"
  result <- .shortest_distance(c("John", "A", "Smith"),
                               c("John", "Andrew", "Smith"))
  expect_true(result > 0.2 && result < 0.3)  # Best alignment still has mismatch

  expect_equal(.shortest_distance(c("J", "R", "R", "Tolkien"),
               c("J", "Tolkien")), 0)
})

test_that(".shortest_distance handles edge cases correctly", {
  # Empty inputs return 1 (maximum distance)
  expect_equal(.shortest_distance(character(0), c("John", "Smith")), 1)
  expect_equal(.shortest_distance(c("John"), character(0)), 1)
  expect_equal(.shortest_distance(character(0), character(0)), 1)

  # All single-character parts filtered out (becomes empty, returns 1)
  expect_equal(.shortest_distance(c("J", "A"), c("John", "Adams")), 1)
  expect_equal(.shortest_distance(c("J", "R", "R"), c("T", "K", "N")), 1)
  expect_equal(.shortest_distance(c("John", "Adams"), c("J", "A")), 1)

  # Mixed initials and full names
  expect_equal(.shortest_distance(c("J", "R", "R", "Tolkien"), c("J", "Tolkien")), 0)
  expect_equal(.shortest_distance(c("J", "Tolkien"), c("J", "R", "R", "Tolkien")), 0)

  # International characters (accents create non-zero distance)
  jose_distance <- .shortest_distance(c("José"), c("Jose"))
  expect_true(jose_distance > 0)
  expect_true(jose_distance < 0.2)  # Should be close but not identical

  # Common name variations have expected distances
  smith_smyth <- .shortest_distance(c("Smith"), c("Smyth"))
  expect_true(smith_smyth > 0.1 && smith_smyth < 0.15)  # ~0.133

  macdonald_mcdonald <- .shortest_distance(c("MacDonald"), c("McDonald"))
  expect_true(macdonald_mcdonald > 0 && macdonald_mcdonald < 0.05)  # ~0.037
})

# .name_distance() ------------------------------------------------------------

test_that(".name_distance works with vectorized targets", {
  # Single target (returns vector of length 1)
  expect_equal(.name_distance("John Smith", c("John Smith")), 0)
  expect_length(.name_distance("John Smith", c("John Smith")), 1)

  # Multiple targets (returns parallel vector)
  result <- .name_distance("John Smith", c("John Smith", "John Smyth", "Jane Smith"))
  expect_length(result, 3)
  expect_equal(result[1], 0)  # Exact match
  expect_true(result[2] > 0 && result[2] < 0.2)  # Close match (Smyth)
  expect_true(result[3] > 0.1)  # Different first name (Jane)

  # Integration with .name_parts() - should give same result as calling directly
  expect_equal(.name_distance("John Smith", c("John Smith")),
               .shortest_distance(.name_parts("John Smith"),
                                .name_parts("John Smith")))
  expect_equal(.name_distance("Mary Jane", c("Mary Jane", "Jane Mary")),
               c(.shortest_distance(.name_parts("Mary Jane"), .name_parts("Mary Jane")),
                 .shortest_distance(.name_parts("Mary Jane"), .name_parts("Jane Mary"))))

  # Handles punctuation (via .name_parts)
  result <- .name_distance("O'Brien", c("O'Brien", "Brien"))
  expect_equal(result[1], 0)  # Exact after splitting
  expect_equal(result[2], 0)  # Just "Brien" matches "Brien" part

  # Threshold behavior for .match_person (< 0.05)
  # Jon Smith vs John Smith: average of Jon~John (0.083) and Smith~Smith (0)
  # = 0.042, which is BELOW 0.05 threshold, so should match
  result <- .name_distance("Jon Smith", c("John Smith"))
  expect_true(result[1] < 0.05)
  expect_true(result[1] > 0)  # But not exact match

  # MacDonald vs McDonald is ~0.037, so SHOULD match
  result <- .name_distance("MacDonald", c("McDonald"))
  expect_true(result[1] < 0.05)
})

test_that(".name_distance handles edge cases correctly", {
  # Empty found name (returns 1 after .name_parts returns character(0))
  expect_equal(.name_distance("", c("John Smith")), 1)
  expect_equal(.name_distance("   ", c("John Smith")), 1)

  # Empty target name (returns 1)
  expect_equal(.name_distance("John Smith", c("")), 1)

  # Multiple targets with mixed empty/valid
  result <- .name_distance("John Smith", c("", "John Smith", "   "))
  expect_equal(result, c(1, 0, 1))

  # Initials-only names filtered to empty (returns 1)
  expect_equal(.name_distance("J A", c("John Adams")), 1)
  expect_equal(.name_distance("John Adams", c("J A")), 1)
  expect_equal(.name_distance("J R R", c("J K R")), 1)

  # Vectorization preserves order
  targets <- c("Alice", "Bob", "Charlie", "David")
  result <- .name_distance("Bob", targets)
  expect_length(result, 4)
  expect_equal(result[2], 0)  # Bob matches Bob (index 2)
  expect_true(all(result[-2] > 0))  # Others don't match

  # More complex order preservation
  targets <- c("John Smith", "Jane Smith", "John Smyth", "Bob Jones")
  result <- .name_distance("John Smith", targets)
  expect_length(result, 4)
  expect_equal(result[1], 0)  # Exact match at index 1
  expect_true(result[2] > result[3])  # Jane Smith farther than John Smyth
  expect_true(result[4] > result[1])  # Bob Jones farther than exact match

  # International names with accents
  result <- .name_distance("José García", c("José García", "Jose Garcia"))
  expect_equal(result[1], 0)  # Exact match
  expect_true(result[2] > 0 && result[2] < 0.3)  # Accents create distance
})


# Tests for .rename_element() ------------------------------------------------

test_that(".rename_element renames matching elements", {
  # Basic exact match rename
  test_list <- list(citation_key = "Smith2020", title = "Test Article")
  result <- .rename_element(test_list, "citation_key", "id")
  expect_equal(names(result), c("id", "title"))
  expect_equal(result$id, "Smith2020")
  expect_equal(result$title, "Test Article")

  # Rename with underscore to hyphen (common CSL conversion)
  test_list <- list(container_title = "Nature", author = "Smith")
  result <- .rename_element(test_list, "container_title", "container-title")
  expect_equal(names(result), c("container-title", "author"))
  expect_equal(result$`container-title`, "Nature")
})

test_that(".rename_element handles pattern matching", {
  # Pattern matching with str_detect - matches substrings
  test_list <- list(event_title = "Conference", event_place = "Boston", title = "Paper")
  result <- .rename_element(test_list, "event", "matched")
  # Both event_title and event_place should match the pattern "event"
  expect_equal(names(result), c("matched", "matched", "title"))
  expect_equal(result[[1]], "Conference")
  expect_equal(result[[2]], "Boston")
  expect_equal(result$title, "Paper")

  # Exact pattern to avoid substring matching
  test_list <- list(page = "123", page_first = "1", title = "Test")
  result <- .rename_element(test_list, "^page$", "pages")
  # Only exact "page" should be renamed, not "page_first"
  expect_equal(names(result), c("pages", "page_first", "title"))
  expect_equal(result$pages, "123")
  expect_equal(result$page_first, "1")
})

test_that(".rename_element returns unchanged list when no matches", {
  # No matching elements
  test_list <- list(title = "Test", author = "Smith", year = 2020)
  result <- .rename_element(test_list, "nonexistent_field", "new_name")
  expect_equal(result, test_list)
  expect_equal(names(result), c("title", "author", "year"))

  # Pattern that doesn't match anything
  test_list <- list(doi = "10.1234", pmid = "12345")
  result <- .rename_element(test_list, "citation", "cite")
  expect_equal(result, test_list)
})

test_that(".rename_element handles edge cases", {
  # Empty list - returns empty list with character(0) names attribute
  result <- .rename_element(list(), "anything", "something")
  expect_length(result, 0)
  expect_true(is.list(result))

  # List with one element
  test_list <- list(single = "value")
  result <- .rename_element(test_list, "single", "renamed")
  expect_equal(names(result), "renamed")
  expect_equal(result$renamed, "value")

  # Rename to same name (no-op)
  test_list <- list(field = "value", other = "data")
  result <- .rename_element(test_list, "field", "field")
  expect_equal(result, test_list)

  # Multiple underscores to hyphens (CSL naming convention)
  test_list <- list(
    original_publisher_place = "NYC",
    original_publisher = "Press",
    title = "Book"
  )
  result <- .rename_element(test_list, "original_publisher_place", "original-publisher-place")
  expect_equal(names(result)[1], "original-publisher-place")
  expect_equal(result$`original-publisher-place`, "NYC")

  # Special characters in values (should not affect renaming)
  test_list <- list(field_name = "Value with $pecial ch@rs!", other = "Normal")
  result <- .rename_element(test_list, "field_name", "renamed-field")
  expect_equal(names(result), c("renamed-field", "other"))
  expect_equal(result$`renamed-field`, "Value with $pecial ch@rs!")
})

test_that(".rename_element preserves list structure and values", {
  # Preserve value types
  test_list <- list(
    text = "string",
    number = 42,
    decimal = 3.14,
    logical = TRUE,
    null_val = NULL,
    na_val = NA
  )
  result <- .rename_element(test_list, "text", "renamed")
  expect_equal(result$renamed, "string")
  expect_equal(result$number, 42)
  expect_equal(result$decimal, 3.14)
  expect_equal(result$logical, TRUE)
  expect_null(result$null_val)
  expect_true(is.na(result$na_val))

  # Preserve order of non-renamed elements
  test_list <- list(first = 1, second = 2, third = 3, fourth = 4)
  result <- .rename_element(test_list, "second", "SECOND")
  expect_equal(names(result), c("first", "SECOND", "third", "fourth"))
  expect_equal(unname(unlist(result)), c(1, 2, 3, 4))
})


# Tests for .is_internal() ----------------------------------------------------

test_that(".is_internal identifies columns ending with _id", {
  # Test various _id suffixed columns
  test_list <- list(
    person_id = "abc-123",
    item_id = "def-456",
    personlist_id = "ghi-789",
    name = "John",
    title = "Article"
  )

  result <- .is_internal(test_list)

  # person_id, item_id, personlist_id should be internal
  expect_true(result[1])  # person_id
  expect_true(result[2])  # item_id
  expect_true(result[3])  # personlist_id

  # name and title should NOT be internal
  expect_false(result[4])  # name
  expect_false(result[5])  # title
})

test_that(".is_internal identifies special internal columns", {
  # Test the special internal field names
  test_list <- list(
    position = 1,
    object_type = "person",
    created = Sys.time(),
    stage = 0,
    revision = 1,
    regular_field = "data"
  )

  result <- .is_internal(test_list)

  # All special fields should be internal
  expect_true(result[1])  # position
  expect_true(result[2])  # object_type
  expect_true(result[3])  # created
  expect_true(result[4])  # stage
  expect_true(result[5])  # revision

  # regular_field should NOT be internal
  expect_false(result[6])
})

test_that(".is_internal correctly identifies non-internal columns", {
  # Test with only non-internal columns
  test_list <- list(
    name = "John",
    title = "Professor",
    age = 45,
    department = "Biology",
    email = "john@example.com"
  )

  result <- .is_internal(test_list)

  # All should be non-internal
  expect_false(result[1])
  expect_false(result[2])
  expect_false(result[3])
  expect_false(result[4])
  expect_false(result[5])

  # Verify all are FALSE
  expect_true(all(result == FALSE))
})

test_that(".is_internal respects keep parameter", {
  # Test that keep parameter prevents fields from being marked as internal
  test_list <- list(
    person_id = "abc-123",
    item_id = "def-456",
    stage = 0,
    revision = 1,
    name = "John"
  )

  # Without keep, person_id, item_id, stage, revision are internal
  result_no_keep <- .is_internal(test_list)
  expect_true(result_no_keep[1])   # person_id
  expect_true(result_no_keep[2])   # item_id
  expect_true(result_no_keep[3])   # stage
  expect_true(result_no_keep[4])   # revision
  expect_false(result_no_keep[5])  # name

  # With keep=c("person_id"), person_id should NOT be internal
  result_keep_person <- .is_internal(test_list, keep=c("person_id"))
  expect_false(result_keep_person[1])  # person_id (kept)
  expect_true(result_keep_person[2])   # item_id
  expect_true(result_keep_person[3])   # stage
  expect_true(result_keep_person[4])   # revision
  expect_false(result_keep_person[5])  # name

  # With keep=c("stage", "revision"), those should NOT be internal
  result_keep_both <- .is_internal(test_list, keep=c("stage", "revision"))
  expect_true(result_keep_both[1])   # person_id
  expect_true(result_keep_both[2])   # item_id
  expect_false(result_keep_both[3])  # stage (kept)
  expect_false(result_keep_both[4])  # revision (kept)
  expect_false(result_keep_both[5])  # name

  # With keep=c("item_id", "stage"), both should NOT be internal
  result_keep_mixed <- .is_internal(test_list, keep=c("item_id", "stage"))
  expect_true(result_keep_mixed[1])   # person_id
  expect_false(result_keep_mixed[2])  # item_id (kept)
  expect_false(result_keep_mixed[3])  # stage (kept)
  expect_true(result_keep_mixed[4])   # revision
  expect_false(result_keep_mixed[5])  # name
})

test_that(".is_internal handles edge cases", {
  # Empty list
  empty_list <- list()
  result_empty <- .is_internal(empty_list)
  expect_equal(length(result_empty), 0)
  expect_true(is.logical(result_empty))

  # List with only internal fields
  all_internal <- list(
    person_id = "123",
    item_id = "456",
    stage = 0,
    revision = 1,
    created = Sys.time()
  )
  result_all_internal <- .is_internal(all_internal)
  expect_true(all(result_all_internal))

  # Single element list
  single_internal <- list(person_id = "123")
  result_single <- .is_internal(single_internal)
  expect_equal(length(result_single), 1)
  expect_true(result_single[1])

  single_non_internal <- list(name = "John")
  result_single_non <- .is_internal(single_non_internal)
  expect_equal(length(result_single_non), 1)
  expect_false(result_single_non[1])
})

test_that(".is_internal works with mixed internal and non-internal fields", {
  # Realistic example mimicking database object
  test_object <- list(
    person_id = "abc-123",
    revision = 2,
    stage = 0,
    created = Sys.time(),
    object_type = "person",
    primary_given_names = "Jane",
    surnames = "Doe",
    other_given_names = "Marie",
    orcid = "0000-0001-2345-6789"
  )

  result <- .is_internal(test_object)

  # Internal fields
  expect_true(result[1])  # person_id
  expect_true(result[2])  # revision
  expect_true(result[3])  # stage
  expect_true(result[4])  # created
  expect_true(result[5])  # object_type

  # Non-internal fields
  expect_false(result[6])  # primary_given_names
  expect_false(result[7])  # surnames
  expect_false(result[8])  # other_given_names
  expect_false(result[9])  # orcid

  # Count: 5 internal, 4 non-internal
  expect_equal(sum(result), 5)
  expect_equal(sum(!result), 4)
})

test_that(".is_internal handles fields that contain but don't end with _id", {
  # Test fields that contain "_id" but don't end with it
  test_list <- list(
    person_id = "123",           # Should be internal (ends with _id)
    person_id_type = "orcid",    # Should NOT be internal (doesn't end with _id)
    valid_identifier = "abc",    # Should NOT be internal (contains "id" but not "_id")
    item_identifier = "def",     # Should NOT be internal
    stage = 0                    # Should be internal
  )

  result <- .is_internal(test_list)

  expect_true(result[1])   # person_id (ends with _id)
  expect_false(result[2])  # person_id_type (doesn't end with _id)
  expect_false(result[3])  # valid_identifier (no _id)
  expect_false(result[4])  # item_identifier (no _id at end)
  expect_true(result[5])   # stage (special field)
})

# .filter_empty() tests -------------------------------------------------------
# NOTE: .filter_empty() appears to be unused in the codebase (grep search found no usage)
# These tests document current behavior - function is a candidate for removal

test_that(".filter_empty removes empty elements from list elements", {
  # Basic case: list with some empty and non-empty vectors
  input <- list(
    a = c(1, 2, 3),
    b = character(0),
    c = c("x", "y")
  )

  result <- .filter_empty(input)

  # Should keep non-empty vectors
  expect_equal(result$a, c(1, 2, 3))
  expect_equal(result$c, c("x", "y"))

  # Should filter out empty vector from b
  expect_length(result$b, 0)
})

test_that(".filter_empty handles nested lists", {
  # Nested list structure
  input <- list(
    outer1 = list(
      inner1 = c(1, 2),
      inner2 = character(0),
      inner3 = c(3)
    ),
    outer2 = list(
      inner4 = numeric(0)
    )
  )

  result <- .filter_empty(input)

  # Check that it operates on top-level list elements
  expect_true(is.list(result$outer1))
  expect_true(is.list(result$outer2))

  # Each element should have been processed by lapply
  expect_length(result$outer1, 3)  # Still has 3 elements
  expect_length(result$outer2, 1)  # Still has 1 element
})

test_that(".filter_empty handles empty list input", {
  result <- .filter_empty(list())

  expect_true(is.list(result))
  expect_length(result, 0)
})

test_that(".filter_empty handles list with all empty elements", {
  input <- list(
    a = character(0),
    b = numeric(0),
    c = integer(0)
  )

  result <- .filter_empty(input)

  # All elements still present but filtered
  expect_length(result, 3)
  expect_length(result$a, 0)
  expect_length(result$b, 0)
  expect_length(result$c, 0)
})

test_that(".filter_empty preserves NULL elements", {
  # NULL has length 0, so should be filtered
  input <- list(
    a = c(1, 2),
    b = NULL,
    c = c(3)
  )

  result <- .filter_empty(input)

  expect_equal(result$a, c(1, 2))
  expect_equal(result$c, c(3))
  # NULL should be filtered (length(NULL) == 0)
  expect_null(result$b)
})
