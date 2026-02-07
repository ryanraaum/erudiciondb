test_that("insert_new_item from repo2cp", {
  for (db in supported_databases()) {
    testdbobj <- make_testdbobj(db)
    expect_no_error(edb_create_tables(testdbobj$con))

    cg_person_id <- expect_no_error(testdbobj$insert_new_object("person", list(
      primary_given_names="Clark", surnames="Griswold"
    )))

    test_item_1 <- list(
      item = list(title="A title", issued="2025-03-10 20:21:47 UTC"),
      author = list(
        list(family="Griswold", given="Clark"),
        list(family="Johnson", given="Eddie")
      ),
      author_affiliation = list(
        c("Food Preservation Corp"),
        c("Chemical Toilet Cleaners, Inc")
      )
    )

    new_item_id <- expect_no_error(testdbobj$insert_new_object("item", test_item_1))
    retrieved_items <- expect_no_condition(testdbobj$retrieve("item", new_item_id))
    expect_equal(length(retrieved_items), 1)
    retrieved_item <- retrieved_items[[1]]
    expect_equal(retrieved_item$title, test_item_1$item$title)

    retrieved_plist <- expect_no_error(testdbobj$retrieve("personlist", new_item_id, by="item_id"))
    expect_equal(length(retrieved_plist), 1)
    plist_id <- expect_no_error(retrieved_plist[[1]]$personlist_id)

    item_persons <- expect_no_error(testdbobj$retrieve("item_person", plist_id, by="personlist_id", as_list=FALSE))
    expect_true(nrow(item_persons) == 2)
    expect_true(cg_person_id %in% item_persons$person_id)
  }
})
