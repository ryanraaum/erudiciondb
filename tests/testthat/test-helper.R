
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


