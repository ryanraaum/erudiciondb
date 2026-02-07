test_that(".revise_object properly updates existing objects", {
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

    expect_equal(.next_revision(testcon, "item", new_item_id), 2)

    revised_item <- expect_no_condition(.revise_object(testcon, new_item, title="Genbank"))
    expect_equal(new_item$title, "GenBank")
    expect_equal(revised_item$title, "Genbank")
  }
})

test_that(".next_revision throws error for non-existent object", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Test with a non-existent UUID
    fake_uuid <- uuid::UUIDgenerate()
    expect_error(.next_revision(testcon, "item", fake_uuid),
                 regexp = "not found")
  }
})

test_that(".next_revision returns correct next revision number", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Insert first revision of an item
    proto_item <- list(title="Test Item", volume=1)
    item <- .new_object(testcon, "item", proto_item)
    item_id <- .insert_one(testcon, item)

    # After revision 1, next should be 2
    expect_equal(.next_revision(testcon, "item", item_id), 2)

    # Insert revision 2 by updating
    item_rev1 <- .retrieve(testcon, "item", item_id)[[1]]
    .update_object(testcon, item_rev1, volume=2)

    # After revision 2, next should be 3
    expect_equal(.next_revision(testcon, "item", item_id), 3)

    # Insert revision 3 by updating again
    item_rev2 <- .retrieve(testcon, "item", item_id)[[1]]
    .update_object(testcon, item_rev2, volume=3)

    # After revision 3, next should be 4
    expect_equal(.next_revision(testcon, "item", item_id), 4)

    # Insert revision 4
    item_rev3 <- .retrieve(testcon, "item", item_id)[[1]]
    .update_object(testcon, item_rev3, volume=4)

    # After revision 4, next should be 5
    expect_equal(.next_revision(testcon, "item", item_id), 5)
  }
})

test_that(".next_revision works for all object types", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Test with person
    proto_person <- list(surnames="Smith", primary_given_names="John")
    person <- .new_object(testcon, "person", proto_person)
    person_id <- .insert_one(testcon, person)
    expect_equal(.next_revision(testcon, "person", person_id), 2)

    # Test with item
    proto_item <- list(title="Article")
    item <- .new_object(testcon, "item", proto_item)
    item_id <- .insert_one(testcon, item)
    expect_equal(.next_revision(testcon, "item", item_id), 2)

    # Test with personlist
    proto_personlist <- list(item_id=item_id, personlist_type="author")
    personlist <- .new_object(testcon, "personlist", proto_personlist)
    personlist_id <- .insert_one(testcon, personlist)
    expect_equal(.next_revision(testcon, "personlist", personlist_id), 2)

    # Test with item_person
    proto_item_person <- list(
      personlist_id=personlist_id,
      person_id=person_id,
      family="Smith",
      given="John"
    )
    item_person <- .new_object(testcon, "item_person", proto_item_person)
    item_person_id <- .insert_one(testcon, item_person)
    expect_equal(.next_revision(testcon, "item_person", item_person_id), 2)

    # Test with person_identifier
    proto_identifier <- list(
      person_id=person_id,
      id_type="orcid",
      id_value="0000-0001-2345-6789"
    )
    identifier <- .new_object(testcon, "person_identifier", proto_identifier)
    identifier_id <- .insert_one(testcon, identifier)
    expect_equal(.next_revision(testcon, "person_identifier", identifier_id), 2)
  }
})

test_that(".next_revision increments correctly after multiple updates", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Create initial person
    proto_person <- list(surnames="Original", primary_given_names="Test")
    person <- .new_object(testcon, "person", proto_person)
    person_id <- .insert_one(testcon, person)

    # Track expected next revision
    expected_next <- 2
    expect_equal(.next_revision(testcon, "person", person_id), expected_next)

    # Perform 5 updates and verify next revision increments each time
    for (i in 1:5) {
      person_current <- .retrieve(testcon, "person", person_id)[[1]]
      .update_object(testcon, person_current, surnames=paste0("Update", i))
      expected_next <- expected_next + 1
      expect_equal(.next_revision(testcon, "person", person_id), expected_next)
    }

    # After 1 initial insert + 5 updates, we have revisions 1-6
    # So next should be 7
    expect_equal(.next_revision(testcon, "person", person_id), 7)
  }
})

test_that(".next_revision works with both active and inactive revisions", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Create item and make multiple revisions
    proto_item <- list(title="Version 1")
    item <- .new_object(testcon, "item", proto_item)
    item_id <- .insert_one(testcon, item)

    # Create revision 2 (revision 1 becomes inactive)
    item_rev1 <- .retrieve(testcon, "item", item_id)[[1]]
    .update_object(testcon, item_rev1, title="Version 2")

    # Create revision 3 (revision 2 becomes inactive)
    item_rev2 <- .retrieve(testcon, "item", item_id)[[1]]
    .update_object(testcon, item_rev2, title="Version 3")

    # Now we have:
    # - Revision 1 (stage=-1, inactive)
    # - Revision 2 (stage=-1, inactive)
    # - Revision 3 (stage=0, active)

    # .next_revision should look at ALL revisions (not just active)
    # So next should be max(1,2,3) + 1 = 4
    expect_equal(.next_revision(testcon, "item", item_id), 4)

    # Verify by checking all revisions exist
    all_revs <- .retrieve(testcon, "item", item_id, stage=-1, revision="all", as_list=FALSE)
    expect_equal(nrow(all_revs), 2)  # 2 inactive revisions

    active_rev <- .retrieve(testcon, "item", item_id, stage=0, revision="all", as_list=FALSE)
    expect_equal(nrow(active_rev), 1)  # 1 active revision

    # Total of 3 revisions, so next is 4
    expect_equal(.next_revision(testcon, "item", item_id), 4)
  }
})

test_that(".destage_one properly destages object in database", {
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

    databased_object_list <- expect_no_condition(.retrieve(testcon, "item", new_item_id))
    expect_equal(length(databased_object_list), 1)
    databased_object <- databased_object_list[[1]]

    expect_true(.destage_one(testcon, databased_object))

    destaged_object <- expect_no_condition(.retrieve(testcon, "item", new_item_id, stage=-1))[[1]]
    expect_equal(destaged_object$stage, "-1")
  }
})

test_that(".destage_one validates required fields", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Test with missing object_type
    bad_object1 <- list(
      revision = 1,
      item_id = uuid::UUIDgenerate()
    )
    expect_error(
      .destage_one(testcon, bad_object1),
      regexp = "object_type"
    )

    # Test with NULL object_type
    bad_object2 <- list(
      object_type = NULL,
      revision = 1,
      item_id = uuid::UUIDgenerate()
    )
    expect_error(
      .destage_one(testcon, bad_object2),
      regexp = "object_type"
    )

    # Test with missing revision
    bad_object3 <- list(
      object_type = "item",
      item_id = uuid::UUIDgenerate()
    )
    expect_error(
      .destage_one(testcon, bad_object3),
      regexp = "revision"
    )

    # Test with NULL revision
    bad_object4 <- list(
      object_type = "item",
      revision = NULL,
      item_id = uuid::UUIDgenerate()
    )
    expect_error(
      .destage_one(testcon, bad_object4),
      regexp = "revision"
    )

    # Test with missing object_id (e.g., item_id)
    bad_object5 <- list(
      object_type = "item",
      revision = 1
    )
    expect_error(
      .destage_one(testcon, bad_object5),
      regexp = "item_id"
    )

    # Test with NULL object_id
    bad_object6 <- list(
      object_type = "item",
      revision = 1,
      item_id = NULL
    )
    expect_error(
      .destage_one(testcon, bad_object6),
      regexp = "item_id"
    )
  }
})

test_that(".update_object does what it should", {
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

    databased_object_list <- expect_no_condition(.retrieve(testcon, "item", new_item_id))
    expect_equal(length(databased_object_list), 1)
    databased_object <- databased_object_list[[1]]

    new_title <- "It's a me, GenBank"
    updated_object_id <- expect_no_condition(.update_object(testcon, databased_object, title=new_title))
    expect_equal(updated_object_id, new_item_id)

    updated_object <- expect_no_condition(.retrieve(testcon, "item", updated_object_id))[[1]]
    expect_equal(updated_object$title, new_title)
  }
})

test_that(".update_object throws informative error on failure", {
  # Bug fix #5: .update_object returned NULL on errors instead of throwing
  # The fix makes it throw informative errors (R/interface.R:86-100)
  # This test verifies errors are thrown, not NULL returned

  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Insert a valid item
    proto_new_item <- list(
      title="Test Item",
      container_title="Test Journal"
    )
    new_item <- .new_object(testcon, "item", proto_new_item)
    new_item_id <- .insert_one(testcon, new_item)

    # Retrieve the item
    databased_item <- .retrieve(testcon, "item", new_item_id)[[1]]

    # Create a malformed update that will fail
    # (item with wrong object_type)
    bad_item <- databased_item
    bad_item$object_type <- "nonexistent_type"

    # Should throw error, not return NULL
    # The exact error message may vary but it should error
    expect_error(
      .update_object(testcon, bad_item, title="Updated Title")
    )

    # Test with missing object_type - should also error
    bad_item2 <- databased_item
    bad_item2$object_type <- NULL

    expect_error(
      .update_object(testcon, bad_item2, title="Updated Title")
    )
  }
})

test_that(".update_object maintains only one active revision", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Create initial item
    proto_new_item <- list(title="Test Item", volume=1)
    new_item <- expect_no_condition(.new_object(testcon, "item", proto_new_item))
    new_item_id <- expect_no_condition(.insert_one(testcon, new_item))

    # Perform update
    databased_object <- expect_no_condition(.retrieve(testcon, "item", new_item_id))[[1]]
    updated_object_id <- expect_no_condition(.update_object(testcon, databased_object, volume=2))

    # Verify only one revision has stage=0
    all_revisions <- expect_no_condition(.retrieve(testcon, "item", new_item_id,
                                                   stage=0, revision="all", as_list=FALSE))
    expect_equal(nrow(all_revisions), 1,
                 info="Only one revision should have stage=0 after update")
    expect_equal(all_revisions$revision, 2)
    expect_equal(all_revisions$volume, "2")

    # Verify old revision is destaged
    old_revisions <- expect_no_condition(.retrieve(testcon, "item", new_item_id,
                                                   stage=-1, revision="all", as_list=FALSE))
    expect_equal(nrow(old_revisions), 1)
    expect_equal(old_revisions$revision, 1)
    expect_equal(old_revisions$volume, "1")
  }
})


# Tests for .make_revision_filter() -------------------------------------------

test_that(".make_revision_filter creates correct filter function for 'max' mode", {
  # .make_revision_filter is a factory that returns filter functions
  # Test that "max" mode creates a function that gets highest revision

  filter_fn <- .make_revision_filter("max")
  expect_true(is.function(filter_fn))

  # Create test data frame with multiple revisions
  test_data <- tibble::tibble(
    item_id = rep("test-id", 3),
    revision = c(1, 2, 3),
    title = c("Rev 1", "Rev 2", "Rev 3")
  )

  # Apply the filter
  result <- filter_fn(test_data)

  # Should return only the highest revision
  expect_equal(nrow(result), 1)
  expect_equal(result$revision, 3)
  expect_equal(result$title, "Rev 3")
})

test_that(".make_revision_filter creates correct filter function for 'all' mode", {
  filter_fn <- .make_revision_filter("all")
  expect_true(is.function(filter_fn))

  # Create test data frame with multiple revisions
  test_data <- tibble::tibble(
    item_id = rep("test-id", 3),
    revision = c(1, 2, 3),
    title = c("Rev 1", "Rev 2", "Rev 3")
  )

  # Apply the filter
  result <- filter_fn(test_data)

  # Should return all rows
  expect_equal(nrow(result), 3)
  expect_equal(result$revision, c(1, 2, 3))
  expect_equal(result$title, c("Rev 1", "Rev 2", "Rev 3"))
})

test_that(".make_revision_filter creates correct filter function for specific revision", {
  # Test with revision 2
  filter_fn <- .make_revision_filter(2)
  expect_true(is.function(filter_fn))

  # Create test data frame with multiple revisions
  test_data <- tibble::tibble(
    item_id = rep("test-id", 3),
    revision = c(1, 2, 3),
    title = c("Rev 1", "Rev 2", "Rev 3")
  )

  # Apply the filter
  result <- filter_fn(test_data)

  # Should return only revision 2
  expect_equal(nrow(result), 1)
  expect_equal(result$revision, 2)
  expect_equal(result$title, "Rev 2")

  # Test with revision 1
  filter_fn_1 <- .make_revision_filter(1)
  result_1 <- filter_fn_1(test_data)
  expect_equal(nrow(result_1), 1)
  expect_equal(result_1$revision, 1)

  # Test with revision 3
  filter_fn_3 <- .make_revision_filter(3)
  result_3 <- filter_fn_3(test_data)
  expect_equal(nrow(result_3), 1)
  expect_equal(result_3$revision, 3)
})

test_that(".make_revision_filter handles edge cases correctly", {
  # Test with single revision
  test_data_single <- tibble::tibble(
    item_id = "test-id",
    revision = 1,
    title = "Only Rev"
  )

  # "max" should return the only row
  filter_max <- .make_revision_filter("max")
  result_max <- filter_max(test_data_single)
  expect_equal(nrow(result_max), 1)
  expect_equal(result_max$revision, 1)

  # "all" should return the only row
  filter_all <- .make_revision_filter("all")
  result_all <- filter_all(test_data_single)
  expect_equal(nrow(result_all), 1)

  # Empty data frame
  test_data_empty <- tibble::tibble(
    item_id = character(0),
    revision = integer(0),
    title = character(0)
  )

  # All filters should return empty
  expect_equal(nrow(filter_max(test_data_empty)), 0)
  expect_equal(nrow(filter_all(test_data_empty)), 0)
  filter_specific <- .make_revision_filter(1)
  expect_equal(nrow(filter_specific(test_data_empty)), 0)

  # Non-existent revision
  test_data <- tibble::tibble(
    item_id = rep("test-id", 3),
    revision = c(1, 2, 3)
  )
  filter_nonexistent <- .make_revision_filter(99)
  result_nonexistent <- filter_nonexistent(test_data)
  expect_equal(nrow(result_nonexistent), 0)
})

test_that(".make_revision_filter integrates correctly with .retrieve", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Create item with initial data
    proto_item <- list(title="Original", volume=1)
    item <- .new_object(testcon, "item", proto_item)
    item_id <- .insert_one(testcon, item)

    # Create multiple revisions through updates
    item_rev1 <- .retrieve(testcon, "item", item_id)[[1]]
    .update_object(testcon, item_rev1, title="Updated Once", volume=2)

    item_rev2 <- .retrieve(testcon, "item", item_id)[[1]]
    .update_object(testcon, item_rev2, title="Updated Twice", volume=3)

    # Now we have 3 revisions (1, 2, 3) with stages (-1, -1, 0)

    # Test "max" mode - should get revision 3 (active)
    retrieved_max <- .retrieve(testcon, "item", item_id, stage=0, revision="max")
    expect_equal(length(retrieved_max), 1)
    expect_equal(as.integer(retrieved_max[[1]]$revision), 3)
    expect_equal(retrieved_max[[1]]$title, "Updated Twice")

    # Test "all" mode with stage=0 - should get only active revision
    retrieved_all_active <- .retrieve(testcon, "item", item_id, stage=0, revision="all", as_list=FALSE)
    expect_equal(nrow(retrieved_all_active), 1)
    expect_equal(as.integer(retrieved_all_active$revision), 3)

    # Test "all" mode with stage=-1 - should get all inactive revisions
    retrieved_all_inactive <- .retrieve(testcon, "item", item_id, stage=-1, revision="all", as_list=FALSE)
    expect_equal(nrow(retrieved_all_inactive), 2)
    expect_equal(sort(as.integer(retrieved_all_inactive$revision)), c(1, 2))

    # Test specific revision - get revision 1 (inactive)
    retrieved_rev1 <- .retrieve(testcon, "item", item_id, stage=-1, revision=1)
    expect_equal(length(retrieved_rev1), 1)
    expect_equal(as.integer(retrieved_rev1[[1]]$revision), 1)
    expect_equal(retrieved_rev1[[1]]$title, "Original")

    # Test specific revision - get revision 2 (inactive)
    retrieved_rev2 <- .retrieve(testcon, "item", item_id, stage=-1, revision=2)
    expect_equal(length(retrieved_rev2), 1)
    expect_equal(as.integer(retrieved_rev2[[1]]$revision), 2)
    expect_equal(retrieved_rev2[[1]]$title, "Updated Once")
  }
})

test_that(".make_revision_filter works with different object types", {
  for (db in supported_databases()) {
    testcon <- make_testcon(db)
    expect_no_error(edb_create_tables(testcon))

    # Test with person object
    proto_person <- list(surnames="Smith", primary_given_names="John")
    person <- .new_object(testcon, "person", proto_person)
    person_id <- .insert_one(testcon, person)

    # Update to create revision 2
    person_rev1 <- .retrieve(testcon, "person", person_id)[[1]]
    .update_object(testcon, person_rev1, surnames="Smith-Jones")

    # Test "max" retrieves latest revision
    retrieved_person <- .retrieve(testcon, "person", person_id, revision="max")
    expect_equal(length(retrieved_person), 1)
    expect_equal(as.integer(retrieved_person[[1]]$revision), 2)
    expect_equal(retrieved_person[[1]]$surnames, "Smith-Jones")

    # Test "all" with stage=-1 gets old revision
    old_person <- .retrieve(testcon, "person", person_id, stage=-1, revision="all", as_list=FALSE)
    expect_equal(nrow(old_person), 1)
    expect_equal(as.integer(old_person$revision), 1)
    expect_equal(old_person$surnames, "Smith")
  }
})
