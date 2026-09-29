test_that("RSWrappedClass correctly wraps a class", {
  load_dummies()
  obj <- DummyClass$new()
  wrapped <- RSWrappedClass$new(obj)
  expect_equal(wrapped$classObject()$classname, "DummyClass")
  expect_equal(wrapped$class_id$value(), "DummyClass_1.2")
  expect_equal(wrapped$className(), "DummyClass")
  expect_equal(wrapped$version(), "1.2")
  unload_dummies()
})

test_that("RSWrappedClass handles NULL version as '0.0'", {
  load_dummies()
  obj <- NoVersionClass$new()
  wrapped <- RSWrappedClass$new(obj)
  expect_equal(wrapped$class_id$value(), "NoVersionClass_0.0")
  expect_equal(wrapped$version(), "0.0")
  unload_dummies()
})

test_that("RSWrappedClass blocks underscore in version", {
  load_dummies()
  obj <- BadVersionClass$new()
  expect_error(RSWrappedClass$new(obj), "Do not contin '_' for version expression")
  unload_dummies()
})
