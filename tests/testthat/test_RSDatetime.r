test_that("RSDateTime: wraps POSIXct correctly with timezone", {
  dt <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")
  rsdt <- RSDateTime$new(dt)
  expect_s3_class(rsdt$value(), "POSIXct")
  expect_identical(attr(rsdt$value(), "tzone"), "UTC")
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

test_that("RSDateTime preserves an instant across POSIX classes and time zones", {
  utc <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")
  new_york <- as.POSIXct("2024-01-01 07:00:00", tz = "America/New_York")
  wrapped <- list(
    utc_ct = RSDateTime_withDateTime(utc),
    new_york_ct = RSDateTime_withDateTime(new_york),
    utc_lt = RSDateTime_withDateTime(as.POSIXlt(utc, tz = "UTC")),
    new_york_lt = RSDateTime$new(as.POSIXlt(new_york, tz = "America/New_York")),
    dispatched_lt = RSScalar_withValue(as.POSIXlt(utc, tz = "UTC"))
  )
  for (value in wrapped) {
    expect_identical(as.numeric(value$value()), as.numeric(utc))
    expect_identical(value$hash(), wrapped[[1L]]$hash())
    expect_identical(value$compare(wrapped[[1L]]), 0L)
  }
  expect_identical(attr(wrapped$utc_ct$value(), "tzone"), "UTC")
  expect_identical(attr(wrapped$utc_lt$value(), "tzone"), "UTC")
  expect_identical(attr(wrapped$dispatched_lt$value(), "tzone"), "UTC")
  expect_identical(attr(wrapped$new_york_ct$value(), "tzone"), "America/New_York")
  expect_identical(attr(wrapped$new_york_lt$value(), "tzone"), "America/New_York")
  expect_identical(format(wrapped$new_york_lt$value()), "2024-01-01 07:00:00")
})

test_that("RSDateTime leaves an unspecified POSIXct time zone unspecified", {
  timestamp <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")
  attr(timestamp, "tzone") <- NULL
  wrapped <- RSDateTime_withDateTime(timestamp)
  expect_null(attr(wrapped$value(), "tzone"))
  expect_identical(as.numeric(wrapped$value()), as.numeric(timestamp))
})

test_that("RSDateTime handles POSIXlt NA and future timestamps", {
  expect_identical(RSDateTime_withDateTime(as.POSIXlt(NA)), RSDateTime$na)
  expect_identical(RSScalar_withValue(as.POSIXlt(NA)), RSDateTime$na)

  future <- RSDateTime_withDateTime(as.POSIXct("2040-01-01", tz = "UTC"))
  expect_type(future$hash(), "integer")
  expect_false(is.na(future$hash()))
  expect_false(is.na(RSDateTime$na$hash()))

  base <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  first_fraction <- RSDateTime_withDateTime(base + 0.25)
  second_fraction <- RSDateTime_withDateTime(base + 0.5)
  expect_false(identical(first_fraction$hash(), second_fraction$hash()))
})

test_that("RSDateTime preserves elapsed time across a DST transition", {
  before <- as.POSIXct("2024-03-10 06:59:59", tz = "UTC")
  after <- as.POSIXct("2024-03-10 07:00:00", tz = "UTC")
  before_lt <- as.POSIXlt(before, tz = "America/New_York")
  after_lt <- as.POSIXlt(after, tz = "America/New_York")
  first <- RSDateTime_withDateTime(before_lt)
  second <- RSDateTime_withDateTime(after_lt)

  expect_identical(as.numeric(first$value()), as.numeric(before))
  expect_identical(as.numeric(second$value()), as.numeric(after))
  expect_identical(as.numeric(second$value()) - as.numeric(first$value()), 1)
})
