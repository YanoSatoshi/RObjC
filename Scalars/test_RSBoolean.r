library(testthat)

test_that("RSBoolean: value() returns correct logical", {
  tf <- RSBoolean$new(TRUE)
  ff <- RSBoolean$new(FALSE)
  expect_true(tf$value())
  expect_false(ff$value())
})

test_that("RSBoolean: asNumber() works as expected", {
  tf <- RSBoolean$new(TRUE)
  ff <- RSBoolean$new(FALSE)
  expect_s3_class(tf$asNumber(), "RSNumber")
  expect_equal(tf$asNumber()$value(), 1)
  expect_equal(ff$asNumber()$value(), 0)
})

test_that("RSBoolean: compare() works correctly", {
  tf <- RSBoolean$new(TRUE)
  ff <- RSBoolean$new(FALSE)
  tf2 <- RSBoolean$new(TRUE)
  expect_equal(ff$compare(tf), -1)
  expect_equal(tf$compare(ff), 1)
  expect_equal(tf$compare(tf2), 0)
})

test_that("RSBoolean: isEqual() detects logical identity", {
  a <- RSBoolean$new(TRUE)
  b <- RSBoolean$new(TRUE)
  c <- RSBoolean$new(FALSE)
  expect_true(a$isEqual(b))
  expect_false(a$isEqual(c))
})

test_that("RSBoolean: hash() returns integer", {
  tf <- RSBoolean$new(TRUE)
  expect_type(tf$hash(), "integer")
})

test_that("RSScalar$valueWith(TRUE/FALSE) returns RSBoolean", {
  x <- RSScalar$valueWith(TRUE)
  y <- RSScalar$valueWith(FALSE)
  expect_s3_class(x, "RSBoolean")
  expect_s3_class(y, "RSBoolean")
})

test_that("RSNABoolean: value is NA", {
  na_bool <- RSNABoolean$new()
  expect_true(is.na(na_bool$value()))
})

test_that("RSNABoolean: compare() with na.last = TRUE/FALSE", {
  tf <- RSBoolean$new(TRUE)
  na <- RSNABoolean$new()
  expect_equal(na$compare(tf, na.last = TRUE), 1)
  expect_equal(na$compare(tf, na.last = FALSE), -1)
  expect_equal(tf$compare(na, na.last = TRUE), -1)
  expect_equal(tf$compare(na, na.last = FALSE), 1)
  expect_equal(na$compare(na, na.last = TRUE), 0)
})

test_that("RSNABoolean: isEqual() true for same NA class", {
  a <- RSNABoolean$new()
  b <- RSNABoolean$new()
  expect_true(a$isEqual(b))
})

test_that("RSNABoolean: hash() returns consistent hash", {
  a <- RSNABoolean$new()
  b <- RSNABoolean$new()
  expect_equal(a$hash(), b$hash())
})
