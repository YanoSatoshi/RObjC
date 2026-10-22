test_that("missing scalar wrappers hash to zero", {
  missing_values <- list(
    RSBoolean$na, RSNumber$na, RSString$na, RSFactor$na,
    RSComplex$na, RSDate$na, RSDateTime$na, RSNull$null
  )
  for (value in missing_values) {
    expect_identical(value$hash(), 0L)
  }
})

test_that("primitive scalar constructors reject S3 classed values", {
  cases <- list(
    list(RSBoolean, RSBoolean_withBoolean, structure(TRUE, class = c("audit", "logical"))),
    list(RSNumber, RSNumber_withValue, structure(1, class = "audit")),
    list(RSComplex, RSComplex_withValue, structure(1 + 0i, class = "audit")),
    list(RSRaw, RSRaw_withValue, structure(as.raw(1), class = "audit"))
  )

  for (case in cases) {
    expect_error(case[[1]]$new(case[[3]]), "S3 class attributes are not supported", fixed = TRUE)
    expect_error(case[[2]](case[[3]]), "S3 class attributes are not supported", fixed = TRUE)
  }

  expect_error(RSBoolean_withBoolean(structure(NA, class = c("audit", "logical"))),
               "S3 class attributes are not supported", fixed = TRUE)
  expect_error(RSNumber_withValue(structure(NA_real_, class = "audit")),
               "S3 class attributes are not supported", fixed = TRUE)
  expect_error(RSComplex_withValue(structure(NA_complex_, class = "audit")),
               "S3 class attributes are not supported", fixed = TRUE)
  expect_identical(RSNumber_withValue(1)$hash(), RSNumber_withValue(1L)$hash())
})

test_that("date, date-time, string, and factor accept only base R classes", {
  date_subclass <- structure(1, class = c("audit", "Date"))
  datetime_subclass <- structure(1, class = c("audit", "POSIXct", "POSIXt"),
                                 tzone = "UTC")
  datetime_lt_subclass <- as.POSIXlt("2020-01-01", tz = "UTC")
  class(datetime_lt_subclass) <- c("audit", "POSIXlt", "POSIXt")
  string_subclass <- structure("x", class = c("audit", "character"))
  factor_subclass <- factor("x", levels = c("x", "y"))
  class(factor_subclass) <- c("audit", "factor")

  expect_error(RSDate$new(date_subclass), "numeric Date", fixed = TRUE)
  expect_error(RSDate_withDate(date_subclass), "Date scalar", fixed = TRUE)
  expect_error(RSDateTime$new(datetime_subclass), "POSIXct or POSIXlt scalar", fixed = TRUE)
  expect_error(RSDateTime_withDateTime(datetime_subclass), "POSIXct or POSIXlt scalar", fixed = TRUE)
  expect_error(RSDateTime$new(datetime_lt_subclass), "POSIXct or POSIXlt scalar", fixed = TRUE)
  expect_error(RSDateTime_withDateTime(datetime_lt_subclass), "POSIXct or POSIXlt scalar", fixed = TRUE)
  expect_error(RSString$new(string_subclass), "unclassed character or standard factor", fixed = TRUE)
  expect_error(RSString_withValue(string_subclass), "character or factor scalar", fixed = TRUE)
  expect_error(RSString_withString(string_subclass), "character or factor scalar", fixed = TRUE)
  expect_error(RSFactor$new(factor_subclass), "factor scalar", fixed = TRUE)
  expect_error(RSFactor_withFactor(factor_subclass), "factor scalar", fixed = TRUE)
  expect_error(RSString_withValue(factor_subclass), "character or factor scalar", fixed = TRUE)

  expect_error(RSDate_withDate(structure(NA_real_, class = c("audit", "Date"))),
               "Date scalar", fixed = TRUE)
  expect_error(RSDateTime_withDateTime(
    structure(NA_real_, class = c("audit", "POSIXct", "POSIXt"), tzone = "UTC")
  ), "POSIXct or POSIXlt scalar", fixed = TRUE)
  expect_error(RSString_withValue(structure(NA_character_, class = c("audit", "character"))),
               "character or factor scalar", fixed = TRUE)
  missing_factor_subclass <- factor(NA_character_, levels = "x")
  class(missing_factor_subclass) <- c("audit", "factor")
  expect_error(RSFactor_withFactor(missing_factor_subclass), "factor scalar", fixed = TRUE)
  expect_error(RSScalar_withValue(date_subclass), "Date scalar", fixed = TRUE)

  expect_true(RSDate_withDate(as.Date("2020-01-01"))$isKindOf(RSDate))
  expect_true(RSDateTime_withDateTime(as.POSIXct("2020-01-01", tz = "UTC"))$isKindOf(RSDateTime))
  expect_true(RSDateTime_withDateTime(as.POSIXlt("2020-01-01", tz = "UTC"))$isKindOf(RSDateTime))
  expect_true(RSString_withValue("x")$isKindOf(RSString))
  expect_true(RSFactor_withFactor(factor("x"))$isKindOf(RSFactor))
  expect_true(RSFactor_withFactor(ordered("x"))$isKindOf(RSFactor))
  expect_identical(RSDate_withDate(as.Date(NA)), RSDate$na)
  expect_identical(RSDateTime_withDateTime(as.POSIXlt(NA)), RSDateTime$na)
  expect_identical(RSString_withValue(NA_character_), RSString$na)
  expect_true(RSFactor_withFactor(factor(NA_character_, levels = "x"))$isKindOf(RSNAFactor))
})

test_that("dates beyond the integer range have a defined hash", {
  far_date <- as.Date(2147483648, origin = "1970-01-01")
  first <- RSDate_withDate(far_date)
  second <- RSDate_withDate(far_date)

  expect_true(first$isEqual(second))
  expect_type(first$hash(), "integer")
  expect_false(is.na(first$hash()))
  expect_identical(first$hash(), second$hash())
})
