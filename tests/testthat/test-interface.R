
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

test_that("object can be inserted and retrieved from database", {
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
                                                        rev = 1))

    expect_true(length(retrieved_people_2) == 1)
    retrieved_person_2 <- retrieved_people_2[[1]]
    expect_equal(new_person_1$primary_given_names, retrieved_person_2$primary_given_names)
  }
})

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
      found_1 <- expect_no_condition(.find_item_by_identifier(testcon,
                                                              doi=this_id,
                                                              pmid=this_id,
                                                              pmcid=this_id))
      found_2 <- expect_no_condition(.find(testcon, "item", list(doi=this_id,
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


test_that("testing stub", {
  for (db in supported_databases()) {
    expect_true(is.character(db))
  }
})
