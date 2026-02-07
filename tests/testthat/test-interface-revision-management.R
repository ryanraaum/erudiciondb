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
