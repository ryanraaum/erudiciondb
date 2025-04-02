
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
