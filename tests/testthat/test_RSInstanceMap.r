test_that("RSInstanceMap initializes and retrieves correctly", {
  load_dummies()
  on.exit(unload_dummies(), add = TRUE)
  obj1 <- DummyClass$new()
  obj2 <- DummyClass$new()
  class_key <- RSWrappedClass$new(obj1)
  map <- RSInstanceMap$new(class_key, list(obj1, obj2))
  expect_s3_class(map, "RSInstanceMap")
  expect_equal(map$classID(), class_key$classID())
  expect_equal(map$wrappedClass()$className(), "DummyClass")
  expect_equal(map$objects()$count(), 2)
})

test_that("RSInstanceMap rejects keys that are not wrapped classes", {
  for (value in list(NULL, 1L, RSObject$new(), RSString_withValue("x"))) {
    expect_error(
      RSInstanceMap$new(value),
      "`class_ref` must be an RSWrappedClass object.",
      fixed = TRUE
    )
  }
})

test_that("RSInstanceMap accepts only instances of its wrapped class", {
  class_key <- RSWrappedClass$new(RSString)
  string <- RSString_withValue("x")
  number <- RSNumber_withValue(1)
  expect_error(
    RSInstanceMap$new(class_key, list(number)),
    "Instances must belong to `class_ref`.", fixed = TRUE
  )
  expect_error(
    RSInstanceMap$new(class_key, list(NULL)),
    "Instances must belong to `class_ref`.", fixed = TRUE
  )
  map <- RSInstanceMap$new(class_key, list(string))
  expect_error(map$addObject(number),
               "Instance must belong to `class_ref`.", fixed = TRUE)
  expect_equal(map$objects()$count(), 1)
})

test_that("RSInstanceMap accepts separate generators with the same class ID", {
  first_class <- RSObjectClass("AuditMapSameID", version = "1.0")
  second_class <- RSObjectClass("AuditMapSameID", version = "1.0")
  other_class <- RSObjectClass("AuditMapOtherID", version = "1.0")
  class_key <- RSWrappedClass$new(first_class)
  second <- second_class$new()
  map <- RSInstanceMap$new(class_key, list(second))
  expect_true(map$objects()$containsObject(second))
  map$addObject(second_class$new())
  expect_equal(map$objects()$count(), 2)
  expect_error(map$addObject(other_class$new()),
               "Instance must belong to `class_ref`.", fixed = TRUE)
})

test_that("RSInstanceMap exposes immutable snapshots", {
  class_key <- RSWrappedClass$new(RSString)
  map <- RSInstanceMap$new(class_key)
  snapshot <- map$objects()
  expect_s3_class(snapshot, "RSSet")
  expect_error(snapshot$addObject(RSString_withValue("x")))
  expect_s3_class(map$value(), "RSSet")
  expect_s3_class(map$objectsForEncoding()[[2L]], "RSSet")
  map$addObject(RSString_withValue("x"))
  expect_equal(snapshot$count(), 0)
  expect_equal(map$objects()$count(), 1)
})


test_that("RSInstanceMap allows adding objects and finalizing", {
  load_dummies()
  on.exit(unload_dummies(), add = TRUE)
  obj1 <- DummyClass$new()
  obj2 <- DummyClass$new()
  obj3 <- DummyClass$new()
  class_key <- RSWrappedClass$new(obj1)
  map <- RSInstanceMap$new(class_key, list(obj1))
  added <- withVisible(map$addObject(obj2))
  expect_identical(added$value, map)
  expect_false(added$visible)
  map$addObject(obj3)
  expect_equal(map$objects()$count(), 3)
  locked <- withVisible(map$lockInstance())
  expect_identical(locked$value, map)
  expect_false(locked$visible)
  expect_s3_class(map$objects(), "RSSet")
  expect_equal(map$objects()$count(), 3)
  expect_error(map$addObject(DummyClass$new()), "RSInstanceMap cannot modify after finalize.")
})
