test_that("RSWrappedClass correctly wraps a class", {
  load_dummies()
  on.exit(unload_dummies(), add = TRUE)
  obj <- DummyClass$new()
  wrapped <- RSWrappedClass$new(obj)
  expect_equal(wrapped$classObject()$classname, "DummyClass")
  expect_equal(wrapped$class_id$value(), "DummyClass_1.2")
  expect_equal(wrapped$className(), "DummyClass")
  expect_equal(wrapped$version(), "1.2")
  expect_identical(RSWrappedClass$new(DummyClass)$classObject(), DummyClass)
})

test_that("RSWrappedClass rejects non-RObjC inputs", {
  for (value in list(NULL, 1L, list(), R6::R6Class("Plain"))) {
    expect_error(
      RSWrappedClass$new(value),
      "`object` must be an RObjC class or instance.",
      fixed = TRUE
    )
  }
})

test_that("RSWrappedClass uses the factory's default version", {
  load_dummies()
  on.exit(unload_dummies(), add = TRUE)
  obj <- DefaultVersionClass$new()
  wrapped <- RSWrappedClass$new(obj)
  expect_equal(wrapped$class_id$value(), "DefaultVersionClass_1.0.0")
  expect_equal(wrapped$version(), "1.0.0")
})

test_that("wrapped classes use class ID for equality and hash", {
  first <- RSWrappedClass$new(RSString)
  same <- RSWrappedClass$new(RSString)
  different <- RSWrappedClass$new(RSNumber)
  expect_true(first$isEqual(same))
  expect_identical(first$hash(), same$hash())
  expect_false(first$isEqual(different))
  expect_false(first$isEqual("RSString_0.0.3 alpha"))

  first_class <- RSObjectClass("AuditSameID", version = "1.0")
  second_class <- RSObjectClass("AuditSameID", version = "1.0")
  expect_false(identical(first_class, second_class))
  expect_true(RSWrappedClass$new(first_class)$isEqual(
    RSWrappedClass$new(second_class)
  ))
})

test_that("class factories reject underscores in versions", {
  expect_error(
    RSObjectClass("BadVersionClass", version = "1_0"),
    "Version string must not contain '_'",
    fixed = TRUE
  )
})
