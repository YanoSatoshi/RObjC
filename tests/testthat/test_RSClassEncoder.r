test_that("RSClassEncoder records instances under factory class versions", {
  load_dummies()
  on.exit(unload_dummies(), add = TRUE)
  encoder <- RSClassEncoder$instance$refresh()
  obj <- DummyClass$new()

  encoder$encodeObject(obj)
  map <- encoder$class_store$objectForKey(RSString_withValue("DummyClass_1.2"))

  expect_true(map$isMemberOf(RSInstanceMap))
  expect_true(map$objects()$containsObject(obj))
  expect_equal(encoder$class_store$count(), 1)
})

test_that("RSClassEncoder does not encode the same object twice", {
  load_dummies()
  on.exit(unload_dummies(), add = TRUE)
  encoder <- RSClassEncoder$instance$refresh()
  obj <- DummyClass$new()

  encoder$encodeObject(obj)
  encoder$encodeObject(obj)

  map <- encoder$class_store$objectForKey(RSString_withValue("DummyClass_1.2"))
  expect_equal(map$objects()$count(), 1)
})
