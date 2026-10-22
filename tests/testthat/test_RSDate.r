test_that("RSDate: wraps Date correctly", {
  d <- as.Date("2020-01-01")
  rsd <- RSDate$new(d)
  expect_equal(rsd$value(), d)
})

test_that("RSDate rejects a Date class on a nonnumeric value", {
  bad <- structure("x", class = "Date")
  expect_error(RSDate$new(bad), "must be a numeric Date", fixed = TRUE)
  expect_error(RSDate_withDate(bad), "must be a numeric Date", fixed = TRUE)
})

test_that("RSDate: compare() works on Date", {
  d1 <- RSDate$new(as.Date("2020-01-01"))
  d2 <- RSDate$new(as.Date("2020-01-02"))
  d3 <- RSDate$new(as.Date("2020-01-02"))
  expect_equal(d1$compare(d2), -1)
  expect_equal(d2$compare(d1), 1)
  expect_equal(d2$compare(d3), 0)
})

test_that("RSDate: isEqual() detects same Date", {
  a <- RSDate$new(as.Date("2023-12-01"))
  b <- RSDate$new(as.Date("2023-12-01"))
  c <- RSDate$new(as.Date("2023-11-30"))
  expect_true(a$isEqual(b))
  expect_false(a$isEqual(c))
})

test_that("RSDate: hash() returns integer", {
  d <- RSDate$new(as.Date("2023-05-10"))
  expect_type(d$hash(), "integer")
})

test_that("RSScalar_withValue(Date) returns RSDate", {
  wrapped <- RSScalar_withValue(as.Date("2024-04-01"))
  expect_true(wrapped$isKindOf(RSDate))
})

test_that("RSNADate: value is NA", {
  na <- RSDate$na
  expect_true(is.na(na$value()))
})

test_that("RSNADate: compare() with na.last = TRUE/FALSE", {
  val <- RSDate$new(as.Date("2023-01-01"))
  na <- RSDate$na
  expect_equal(na$compare(val, na.last = TRUE), 1)
  expect_equal(na$compare(val, na.last = FALSE), -1)
  expect_equal(val$compare(na, na.last = TRUE), -1)
  expect_equal(val$compare(na, na.last = FALSE), 1)
  expect_equal(na$compare(na, na.last = TRUE), 0)
})

test_that("RSNADate: isEqual() true for same NA class", {
  a <- RSDate$na
  b <- RSDate$na
  expect_true(a$isEqual(b))
})


test_that("RSNADate: hash() stable", {
  a <- RSDate$na
  b <- RSDate$na
  expect_equal(a$hash(), b$hash())
})
