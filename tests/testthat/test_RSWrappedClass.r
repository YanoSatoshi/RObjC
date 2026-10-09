test_that("RSWrappedClass correctly wraps a class", {
  load_dummies()
  on.exit(unload_dummies(), add = TRUE)
  obj <- DummyClass$new()
  wrapped <- RSWrappedClass$new(obj)
  expect_equal(wrapped$classObject()$classname, "DummyClass")
  expect_equal(wrapped$class_id$value(), "DummyClass_1.2")
  expect_equal(wrapped$className(), "DummyClass")
  expect_equal(wrapped$version(), "1.2")
})

test_that("RSWrappedClass uses the factory's default version", {
  load_dummies()
  on.exit(unload_dummies(), add = TRUE)
  obj <- DefaultVersionClass$new()
  wrapped <- RSWrappedClass$new(obj)
  expect_equal(wrapped$class_id$value(), "DefaultVersionClass_1.0.0")
  expect_equal(wrapped$version(), "1.0.0")
})

test_that("class factories reject underscores in versions", {
  expect_error(
    RSObjectClass("BadVersionClass", version = "1_0"),
    "Version string must not contain '_'",
    fixed = TRUE
  )
})
