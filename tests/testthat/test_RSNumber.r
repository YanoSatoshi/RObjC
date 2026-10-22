test_that("RSNumber: value() returns original numeric", {
  num <- RSNumber$new(42)
  expect_equal(num$value(), 42)
})

test_that("RSNumber: compare() gives correct ordering", {
  n1 <- RSNumber$new(1)
  n2 <- RSNumber$new(2)
  n3 <- RSNumber$new(2)
  expect_equal(n1$compare(n2), -1)
  expect_equal(n2$compare(n1), 1)
  expect_equal(n2$compare(n3), 0)
})

test_that("RSNumber: isEqual() detects equality", {
  a <- RSNumber$new(100)
  b <- RSNumber$new(100)
  c <- RSNumber$new(200)
  expect_true(a$isEqual(b))
  expect_false(a$isEqual(c))
})

test_that("RSNumber: hash() generates integer", {
  val <- RSNumber$new(7)
  h <- val$hash()
  expect_type(h, "integer")
})

test_that("RSScalar_withValue() dispatches numeric correctly", {
  wrapped <- RSScalar_withValue(3.14)
  expect_true(wrapped$isKindOf(RSNumber))
  expect_equal(wrapped$value(), 3.14)
})

test_that("RSNANumber: value is NA", {
  nanum <- RSNumber$na
  expect_true(is.na(nanum$value()))
})

test_that("RSNANumber: compare() with na.last = TRUE/FALSE", {
  num <- RSNumber$new(1)
  na <- RSNumber$na
  expect_equal(na$compare(num, na.last = TRUE), 1)
  expect_equal(na$compare(num, na.last = FALSE), -1)
  expect_equal(num$compare(na, na.last = TRUE), -1)
  expect_equal(num$compare(na, na.last = FALSE), 1)
  expect_equal(na$compare(na, na.last = TRUE), 0)
})

test_that("RSNANumber: isEqual() true for same NA class", {
  a <- RSNumber$na
  b <- RSNumber$na
  expect_true(a$isEqual(b))
})

test_that("RSNANumber: hash() returns fixed hash", {
  a <- RSNumber$na
  b <- RSNumber$na
  expect_equal(a$hash(), b$hash())
})

test_that("RSScalar_withValue(as.numeric(NA)) returns RSNANumber", {
  val <- RSScalar_withValue(as.numeric(NA))
  expect_true(val$isKindOf(RSNANumber))
})
