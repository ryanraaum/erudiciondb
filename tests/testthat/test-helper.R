
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


