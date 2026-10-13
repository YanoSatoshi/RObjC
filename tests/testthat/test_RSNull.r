test_that("RSNull: null() returns RSNull instance", {
  n1 <- RSNull$null
  expect_s3_class(n1, "RSNull")
})

test_that("RSNull: value() returns NULL", {
  n <- RSNull$null
  expect_null(n$value())
})

test_that("RSNull: isEqual() returns TRUE for same instance", {
  a <- RSNull$null
  b <- RSNull$null
  expect_true(a$isEqual(b))  # Singleton = 常に同一インスタンス
})

test_that("RSNull: hash() is always 0", {
  a <- RSNull$null
  b <- RSNull$null
  expect_equal(a$hash(), 0L)
  expect_equal(a$hash(), b$hash())
})

test_that("RSNull: singleton identity is enforced", {
  a <- RSNull$null
  b <- RSNull$null
  expect_identical(a, b)
})

test_that("RSNull: direct construction points to the shared instance", {
  expect_error(RSNull$new(), "Use `RSNull$null`. Do not call `RSNull$new()`.",
               fixed = TRUE)
  expect_identical(RSNull$null, RSNull$instance)
})

test_that("RSScalar_withValue(NULL) returns RSNull", {
  wrapped <- RSScalar_withValue(NULL)
  expect_true(wrapped$isKindOf(RSNull))
})
