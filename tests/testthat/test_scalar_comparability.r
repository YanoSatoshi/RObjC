test_that("RSScalar is abstract but its subclasses initialize", {
  for (value in list(1L, "x", as.Date("2024-01-01"),
                     as.POSIXct("2024-01-01", tz = "UTC"))) {
    expect_error(RSScalar$new(value), "RSScalar is abstract", fixed = TRUE)
  }

  expect_true(RSNumber$new(1L)$isKindOf(RSScalar))
  expect_true(RSDate$new(as.Date("2024-01-01"))$isKindOf(RSScalar))
  expect_true(RSDateTime$new(as.POSIXct("2024-01-01", tz = "UTC"))$isKindOf(RSScalar))
})

test_that("concrete scalar comparison uses class relationships", {
  expect_true(RSNumber$new(1L)$isEqual(RSNumber$new(1)))
  expect_false(RSNumber$new(1L)$isEqual(RSBoolean$new(TRUE)))
  expect_error(RSNumber$new(1L)$compare(RSBoolean$new(TRUE)),
               "Scalar classes are not comparable", fixed = TRUE)
  expect_true(RSString$new("a")$isEqual(
    RSFactor$new(factor("a", levels = c("a", "b")))))
})
