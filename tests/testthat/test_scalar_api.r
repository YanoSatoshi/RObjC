test_that("scalar APIs route native values and missing values", {
  expect_true(RSScalar_withValue(NULL)$isKindOf(RSNull))
  expect_true(RSScalar_withValue(TRUE)$isKindOf(RSBoolean))
  expect_true(RSScalar_withValue(1L)$isKindOf(RSNumber))
  expect_true(RSScalar_withValue("x")$isKindOf(RSString))
  expect_true(RSScalar_withValue(factor("x"))$isKindOf(RSFactor))
  expect_true(RSScalar_withValue(as.Date("2024-01-01"))$isKindOf(RSDate))
  expect_true(RSScalar_withValue(as.POSIXct("2024-01-01", tz = "UTC"))$isKindOf(RSDateTime))

  expect_true(RSBoolean_withBoolean(NA)$isMemberOf(RSNABoolean))
  expect_true(RSNumber_withValue(NA_real_)$isMemberOf(RSNANumber))
  expect_true(RSString_withValue(NA_character_)$isMemberOf(RSNAString))
  expect_true(RSFactor_withFactor(factor(NA_character_))$isMemberOf(RSNAFactor))
  expect_true(RSDate_withDate(as.Date(NA))$isMemberOf(RSNADate))
  expect_true(RSDateTime_withDateTime(as.POSIXct(NA))$isMemberOf(RSNADateTime))
})

test_that("Date and date-time NaN values survive direct and convenience construction", {
  nan_date <- as.Date(NaN, origin = "1970-01-01")
  nan_time <- as.POSIXct(NaN, origin = "1970-01-01", tz = "UTC")

  for (wrapped in list(
    RSDate$new(nan_date), RSDate_withDate(nan_date),
    RSScalar_withValue(nan_date)
  )) {
    expect_true(wrapped$isMemberOf(RSDate))
    expect_true(is.nan(wrapped$nativeValue()))
    expect_false(wrapped$isEqual(RSDate$na))
  }
  for (native in list(nan_time, as.POSIXlt(nan_time))) {
    for (wrapped in list(
      RSDateTime$new(native), RSDateTime_withDateTime(native),
      RSScalar_withValue(native)
    )) {
      expect_true(wrapped$isMemberOf(RSDateTime))
      expect_true(is.nan(wrapped$nativeValue()))
      expect_false(wrapped$isEqual(RSDateTime$na))
    }
  }

  expect_identical(RSDate_withDate(as.Date(NA)), RSDate$na)
  expect_identical(RSDateTime_withDateTime(as.POSIXct(NA)), RSDateTime$na)
  expect_identical(RSDateTime_withDateTime(as.POSIXlt(NA)), RSDateTime$na)
})

test_that("Date and date-time APIs reject malformed missing storage", {
  invalid_date <- structure(NA_character_, class = "Date")
  invalid_time <- structure(NA_character_, class = c("POSIXct", "POSIXt"))

  expect_error(RSDate_withDate(invalid_date),
               "`val` must be a numeric Date.", fixed = TRUE)
  expect_error(RSDateTime_withDateTime(invalid_time),
               "`val` must be a numeric POSIX date-time.", fixed = TRUE)
  expect_identical(RSDate_withDate(as.Date(NA)), RSDate$na)
  expect_identical(RSDateTime_withDateTime(as.POSIXlt(NA)), RSDateTime$na)
})

test_that("scalar APIs reject classed values their concrete classes reject", {
  invalid_boolean <- structure(NA, class = "customLogical")
  invalid_string <- structure(NA_character_, class = "customString")

  expect_error(RSBoolean_withBoolean(invalid_boolean), "logical scalar", fixed = TRUE)
  expect_error(RSString_withValue(invalid_string),
               "character or factor scalar", fixed = TRUE)
  expect_error(RSScalar_withValue(invalid_boolean), "logical scalar", fixed = TRUE)
  expect_error(RSScalar_withValue(invalid_string),
               "character or factor scalar", fixed = TRUE)
})

test_that("number and complex NaN values remain regular scalars", {
  values <- list(
    list(NaN, RSNumber, RSNumber$new, RSNumber_withValue),
    list(complex(real = NaN, imaginary = 1), RSComplex,
         RSComplex$new, RSComplex_withValue)
  )
  for (case in values) {
    for (wrapped in list(
      case[[3L]](case[[1L]]), case[[4L]](case[[1L]]),
      RSScalar_withValue(case[[1L]])
    )) {
      expect_true(wrapped$isMemberOf(case[[2L]]))
      expect_true(is.nan(wrapped$nativeValue()))
      expect_false(wrapped$isEqual(case[[2L]]$na))
    }
  }
})

test_that("singleton NA companion classes reject direct construction", {
  cases <- list(
    list(RSNABoolean, NA),
    list(RSNANumber, NA_real_),
    list(RSNAString, NA_character_),
    list(RSNAComplex, NA_complex_),
    list(RSNADate, as.Date(NA)),
    list(RSNADateTime, as.POSIXct(NA))
  )
  for (case in cases) {
    na_class <- case[[1L]]
    expect_error(na_class$new(case[[2L]]), "is a singleton", fixed = TRUE)
    expect_true(bindingIsLocked("new", na_class))
  }
  expect_false(bindingIsLocked("new", RSNAFactor))
})

test_that("typed scalar APIs validate their inputs", {
  expect_error(RSBoolean_withBoolean(1L), "logical scalar")
  expect_error(RSNumber_withValue(TRUE), "integer or double scalar")
  expect_error(RSNumber_withInteger(1.5), "integer scalar")
  expect_error(RSNumber_withDouble(1L), "double scalar")
  expect_error(RSString_withValue(c("a", "b")), "character or factor scalar")
  expect_error(RSFactor_withFactor("a"), "factor scalar")
  expect_error(RSDate_withDate(Sys.time()), "Date scalar")
  expect_error(RSDateTime_withDateTime(Sys.Date()), "POSIXct or POSIXlt scalar")
  expect_error(RSScalar_withValue(list(1)), "Unsupported scalar type")
})

test_that("direct scalar construction checks type before NA routing", {
  expect_error(RSBoolean$new(NA_character_), "class logical", fixed = TRUE)
  expect_error(RSNumber$new(NA_character_), "class numeric", fixed = TRUE)
  expect_error(RSComplex$new(NA_real_), "complex scalar", fixed = TRUE)
  expect_error(RSRaw$new(NA_integer_), "raw scalar", fixed = TRUE)

  expect_error(RSBoolean$new(NA), "RSNABoolean", fixed = TRUE)
  expect_error(RSNumber$new(NA_real_), "RSNANumber", fixed = TRUE)
  expect_error(RSComplex$new(NA_complex_), "RSNAComplex", fixed = TRUE)
})

test_that("scalar constructors reject dimensioned length-one values", {
  factor_matrix <- structure(factor("a", levels = c("a", "b")),
                             dim = c(1L, 1L))
  date_matrix <- structure(as.Date("2024-01-01"), dim = c(1L, 1L))
  time_matrix <- structure(as.POSIXct("2024-01-01", tz = "UTC"),
                           dim = c(1L, 1L))
  cases <- list(
    list(matrix(TRUE, 1L, 1L), RSBoolean_withBoolean, RSBoolean$new),
    list(matrix(1L, 1L, 1L), RSNumber_withValue, RSNumber$new),
    list(matrix(1 + 1i, 1L, 1L), RSComplex_withValue, RSComplex$new),
    list(matrix(as.raw(1), 1L, 1L), RSRaw_withValue, RSRaw$new),
    list(matrix("a", 1L, 1L), RSString_withValue, RSString$new),
    list(factor_matrix, RSFactor_withFactor, RSFactor$new),
    list(date_matrix, RSDate_withDate, RSDate$new),
    list(time_matrix, RSDateTime_withDateTime, RSDateTime$new)
  )
  for (case in cases) {
    expect_error(RSScalar_withValue(case[[1L]]), "scalar", fixed = TRUE)
    expect_error(case[[2L]](case[[1L]]), "scalar", fixed = TRUE)
    expect_error(case[[3L]](case[[1L]]))
  }

  expect_error(RSNumber_withValue(matrix(NA_real_, 1L, 1L)),
               "scalar", fixed = TRUE)
  expect_error(RSString_withValue(matrix(NA_character_, 1L, 1L)),
               "scalar", fixed = TRUE)
  expect_error(RSFactor_withFactor(structure(
    factor(NA_character_, levels = "a"), dim = c(1L, 1L)
  )), "scalar", fixed = TRUE)
})

test_that("legacy scalar class constructors are removed", {
  expect_null(RSScalar$valueWith)
  expect_null(RSBoolean$boolWith)
  expect_null(RSNumber$numberWith)
  expect_null(RSString$stringWith)
  expect_null(RSFactor$factorWith)
  expect_null(RSDate$dateWith)
  expect_null(RSDateTime$dateTimeWith)
})
