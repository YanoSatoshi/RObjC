test_that("RSNull: null() returns RSNull instance", {
  n1 <- RSNull$null
  expect_s3_class(n1, "RSNull")
})

test_that("RSNull: value() returns NULL", {
  n <- RSNull$null
  expect_null(n$value())
})

test_that("RSNull: description() returns one printable string", {
  description <- RSNull$null$description()
  expect_type(description, "character")
  expect_length(description, 1L)
  expect_match(description, "^<RSNull 0x[[:xdigit:]]+> NULL$")
})

test_that("RSNull: isEqual() returns TRUE for same instance", {
  a <- RSNull$null
  b <- RSNull$null
  expect_true(a$isEqual(b))  # Singleton = 常に同一インスタンス
})

test_that("RSNull compares only with its shared instance", {
  null <- RSNull$null
  expect_identical(null$compare(null), 0L)
  expect_identical(null$compare(null, na.last = 0L), 0L)
  expect_error(null$compare(RSNumber$new(1L)),
               "Null objects are not comparable", fixed = TRUE)
  expect_error(null$compare(null, na.last = NA),
               "`na.last` must be TRUE or FALSE", fixed = TRUE)
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
  expect_identical(RSNull$new(), RSNull$null)
  expect_identical(RSNull$new(), RSNull$null)
  expect_identical(RSNull$null, RSNull$instance)
})

test_that("RSScalar_withValue(NULL) returns RSNull", {
  wrapped <- RSScalar_withValue(NULL)
  expect_true(wrapped$isKindOf(RSNull))
})

test_that("RSNull forwards unknown messages to the shared singleton", {
  null <- RSNull$null
  expect_identical(null$value(), NULL)
  expect_identical(null$hash(), 0L)
  expect_false(null$respondsTo("missingMethod"))
  expect_true(is.function(null$missingMethod))
  expect_identical(null$missingMethod(1, option = "x"), null)
  expect_identical(null$missingMethod()$anotherMethod(), null)
})

test_that("robjc_wrap_null prepares collection items", {
  expect_identical(robjc_wrap_null(NULL), RSNull$null)
  object <- RSNumber$new(1L)
  expect_identical(robjc_wrap_null(object), object)
  expect_identical(robjc_wrap_null(RSNumber), RSNumber)
  expect_error(robjc_wrap_null(1L), "Unsupported object type: integer", fixed = TRUE)
})
