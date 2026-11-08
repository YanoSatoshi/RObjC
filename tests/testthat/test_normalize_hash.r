test_that("robjc_normalize_hash maps only a missing hash to zero", {
  expect_identical(robjc_normalize_hash(NA_integer_), 0L)
  expect_identical(robjc_normalize_hash(42L), 42L)
  expect_identical(robjc_normalize_hash(-42L), -42L)
})

test_that("an empty array has a normalized standard hash", {
  expect_identical(RSMutableArray$new()$hash(), 0L)
})
