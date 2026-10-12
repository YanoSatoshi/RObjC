test_that("RSDateTime: wraps POSIXct correctly with timezone", {
  dt <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")
  rsdt <- RSDateTime$new(dt)
  expect_s3_class(rsdt$value(), "POSIXct")
  expect_equal(format(rsdt$value(), tz = "Asia/Tokyo"), "2024-01-01 21:00:00")
})

test_that("RSDateTime: compare() works correctly", {
  d1 <- RSDateTime$new(as.POSIXct("2023-01-01 00:00:00"))
  d2 <- RSDateTime$new(as.POSIXct("2023-01-01 12:00:00"))
  d3 <- RSDateTime$new(as.POSIXct("2023-01-01 12:00:00"))
  expect_equal(d1$compare(d2), -1)
  expect_equal(d2$compare(d1), 1)
  expect_equal(d2$compare(d3), 0)
})

test_that("RSDateTime: isEqual() detects equality", {
  a <- RSDateTime$new(as.POSIXct("2025-04-01 08:30:00"))
  b <- RSDateTime$new(as.POSIXct("2025-04-01 08:30:00"))
  c <- RSDateTime$new(as.POSIXct("2025-04-01 08:31:00"))
  expect_true(a$isEqual(b))
  expect_false(a$isEqual(c))
})

test_that("RSDateTime: hash() returns integer", {
  dt <- RSDateTime$new(as.POSIXct("2024-12-31 23:59:59"))
  expect_type(dt$hash(), "integer")
})

test_that("RSScalar_withValue(POSIXct) returns RSDateTime", {
  dt <- as.POSIXct("2025-01-01 00:00:00")
  wrapped <- RSScalar_withValue(dt)
  expect_true(wrapped$isKindOf(RSDateTime))
})

test_that("RSNADateTime: value is NA POSIXct", {
  na <- RSDateTime$na
  expect_true(is.na(na$value()))
})

test_that("RSNADateTime: compare() with na.last = TRUE/FALSE", {
  val <- RSDateTime$new(as.POSIXct("2023-01-01 00:00:00"))
  na <- RSDateTime$na
  expect_equal(na$compare(val, na.last = TRUE), 1)
  expect_equal(na$compare(val, na.last = FALSE), -1)
  expect_equal(val$compare(na, na.last = TRUE), -1)
  expect_equal(val$compare(na, na.last = FALSE), 1)
  expect_equal(na$compare(na, na.last = TRUE), 0)
})

test_that("RSNADateTime: isEqual() true for same NA class", {
  a <- RSDateTime$na
  b <- RSDateTime$na
  expect_true(a$isEqual(b))
})


test_that("RSNADateTime: hash() is consistent", {
  a <- RSDateTime$na
  b <- RSDateTime$na
  expect_equal(a$hash(), b$hash())
})
