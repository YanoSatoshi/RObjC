library(testthat)

test_that("RSString: value() returns original string", {
  str <- RSString$new("hello")
  expect_equal(str$value(), "hello")
})

test_that("RSString: compare() respects lexicographic order", {
  a <- RSString$new("apple")
  b <- RSString$new("banana")
  c <- RSString$new("banana")
  expect_equal(a$compare(b), -1)
  expect_equal(b$compare(a), 1)
  expect_equal(b$compare(c), 0)
})

test_that("RSString: isEqual() matches identical string", {
  a <- RSString$new("test")
  b <- RSString$new("test")
  c <- RSString$new("diff")
  expect_true(a$isEqual(b))
  expect_false(a$isEqual(c))
})

test_that("RSString: hash() returns consistent integer", {
  a <- RSString$new("hashme")
  b <- RSString$new("hashme")
  expect_type(a$hash(), "integer")
  expect_equal(a$hash(), b$hash())
})

test_that("RSScalar$valueWith('char') returns RSString", {
  wrapped <- RSScalar$valueWith("X")
  expect_true(wrapped$isKindOf(RSString))
  expect_equal(wrapped$value(), "X")
})

test_that("RSNAString: value() returns NA_character_", {
  na_str <- RSNAString$new()
  expect_true(is.na(na_str$value()))
})

test_that("RSNAString: compare() with na.last = TRUE/FALSE", {
  s <- RSString$new("abc")
  na <- RSNAString$new()
  expect_equal(na$compare(s, na.last = TRUE), 1)
  expect_equal(na$compare(s, na.last = FALSE), -1)
  expect_equal(s$compare(na, na.last = TRUE), -1)
  expect_equal(s$compare(na, na.last = FALSE), 1)
  expect_equal(na$compare(na, na.last = TRUE), 0)
})

test_that("RSNAString: isEqual() true for same NA class", {
  a <- RSNAString$new()
  b <- RSNAString$new()
  expect_true(a$isEqual(b))
})


test_that("RSNAString: hash() consistent for all NA instances", {
  a <- RSNAString$new()
  b <- RSNAString$new()
  expect_equal(a$hash(), b$hash())
})
