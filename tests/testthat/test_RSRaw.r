test_that("RSRaw wraps every raw byte without an NA companion", {
  zero <- RSRaw_withValue(as.raw(0))
  high <- RSScalar_withValue(as.raw(255))
  expect_true(zero$isMemberOf(RSRaw))
  expect_true(high$isMemberOf(RSRaw))
  expect_identical(zero$value(), as.raw(0))
  expect_identical(high$value(), as.raw(255))
  expect_false(RSRaw$isNAClass)
  expect_null(RSRaw$na)
})

test_that("RSRaw has key equality and hash but no base R sort order", {
  first <- RSRaw$new(as.raw(16))
  same <- RSRaw$new(as.raw(16))
  later <- RSRaw$new(as.raw(17))
  expect_true(first$isEqual(same))
  expect_false(first$isEqual(later))
  expect_error(first$compare(later), "Raw values cannot be sorted", fixed = TRUE)
  expect_error(later$compare(first), "Raw values cannot be sorted", fixed = TRUE)
  expect_identical(first$hash(), same$hash())
  expect_type(first$hash(), "integer")
})

test_that("RSRaw rejects other types and non-scalars", {
  expect_error(RSRaw$new(1L), "raw scalar", fixed = TRUE)
  expect_error(RSRaw_withValue(as.raw(c(1, 2))), "raw scalar", fixed = TRUE)
  expect_error(RSRaw$new(as.raw(1))$compare(RSNumber$new(1L)),
               "Raw values cannot be sorted", fixed = TRUE)
})
