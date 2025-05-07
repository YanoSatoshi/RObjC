library(testthat)

test_that("RSFactor: wraps factor correctly", {
  f <- factor("low", levels = c("low", "medium", "high"), ordered = TRUE)
  rsf <- RSFactor$new(f)
  expect_equal(rsf$value(), "low")
  expect_equal(rsf$levels(), levels(f))
  expect_true(rsf$ordered())
})

test_that("RSFactor: compare() respects factor ordering", {
  a <- RSFactor$new(factor("low", levels = c("low", "medium", "high"), ordered = TRUE))
  b <- RSFactor$new(factor("medium", levels = c("low", "medium", "high"), ordered = TRUE))
  c <- RSFactor$new(factor("medium", levels = c("low", "medium", "high"), ordered = TRUE))
  expect_equal(a$compare(b), -1)
  expect_equal(b$compare(a), 1)
  expect_equal(b$compare(c), 0)
})

test_that("RSFactor: isEqual() true for same level/value", {
  a <- RSFactor$new(factor("high", levels = c("low", "medium", "high")))
  b <- RSFactor$new(factor("high", levels = c("low", "medium", "high")))
  c <- RSFactor$new(factor("medium", levels = c("low", "medium", "high")))
  expect_true(a$isEqual(b))
  expect_false(a$isEqual(c))
})

test_that("RSFactor: hash() returns consistent int", {
  a <- RSFactor$new(factor("medium", levels = c("low", "medium", "high")))
  b <- RSFactor$new(factor("medium", levels = c("low", "medium", "high")))
  expect_equal(a$hash(), b$hash())
})

test_that("RSScalar$valueWith(factor) returns RSFactor", {
  f <- factor("foo", levels = c("foo", "bar"))
  wrapped <- RSScalar$valueWith(f)
  expect_true(wrapped$isKindOf(RSFactor))
})

test_that("RSNAFactor: value is NA with factor() wrapper", {
  na <- RSNAFactor$new()
  expect_true(is.na(na$value()))
  expect_null(na$levels())
  expect_false(na$ordered())
})

test_that("RSNAFactor: compare() with na.last = TRUE/FALSE", {
  f <- factor("bar", levels = c("foo", "bar", "baz"))
  val <- RSFactor$new(f)
  na <- RSNAFactor$new()
  expect_equal(na$compare(val, na.last = TRUE), 1)
  expect_equal(na$compare(val, na.last = FALSE), -1)
  expect_equal(val$compare(na, na.last = TRUE), -1)
  expect_equal(val$compare(na, na.last = FALSE), 1)
  expect_equal(na$compare(na, na.last = TRUE), 0)
})

test_that("RSNAFactor: isEqual() true for same NA class", {
  a <- RSNAFactor$new()
  b <- RSNAFactor$new()
  expect_true(a$isEqual(b))
})

test_that("RSNAFactor: hash() is stable", {
  a <- RSNAFactor$new()
  b <- RSNAFactor$new()
  expect_equal(a$hash(), b$hash())
})
