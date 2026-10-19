test_that("registry collects standard classes on first lookup", {
  registry <- RSClassRegistry$instance
  expect_true(registry$isStandardClass("RSString"))
  expect_false(registry$isStandardClass("NotAStandardClass"))
  expect_false("std_classes" %in% names(registry))
})
