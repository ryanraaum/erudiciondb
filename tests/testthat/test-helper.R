
test_that(".this_exists finds true things", {
  expect_true(.this_exists("a"))
  expect_true(.this_exists(c("a", "b"))) # single response regardless of length of input
  expect_true(.this_exists(0))
  expect_true(.this_exists(list(a=1)))
})

test_that(".this_exists finds false things", {
  expect_false(.this_exists(NA))
  expect_false(.this_exists(NULL))
  expect_false(.this_exists(list()))
  expect_false(.this_exists(c()))
})

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
