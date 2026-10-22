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

test_that("RSClassEncoder rejects invalid roots without clearing prior work", {
  encoder <- RSClassEncoder$instance$refresh()
  on.exit(encoder$refresh(), add = TRUE)
  object <- RSString_withValue("keep")
  encoder$encodeObject(object)
  class_id <- RSString_withValue(
    paste0(RSString$classname, "_", RSString$robjc_isa$version)
  )
  map <- encoder$class_store$objectForKey(class_id)

  for (invalid in list(NULL, 1L, list(), RSString)) {
    expect_error(encoder$encodeObject(invalid),
                 "`obj` must be an RObjC instance.", fixed = TRUE)
    expect_error(encoder$encodingContainer(invalid),
                 "`obj` must be an RObjC instance.", fixed = TRUE)
  }
  expect_equal(encoder$class_store$count(), 1)
  expect_identical(encoder$class_store$objectForKey(class_id), map)
  expect_true(map$objects()$containsObject(object))
  expect_true(exists(object$objectID(), envir = encoder$visited_env,
                     inherits = FALSE))
})

test_that("RSClassEncoder groups generators with the same class ID", {
  first_class <- RSObjectClass("AuditEncodeSameID", version = "1.0")
  second_class <- RSObjectClass("AuditEncodeSameID", version = "1.0")
  first <- first_class$new()
  second <- second_class$new()
  encoder <- RSClassEncoder$instance$refresh()
  on.exit(encoder$refresh(), add = TRUE)

  encoder$encodeObject(first)
  encoder$encodeObject(second)
  map <- encoder$class_store$objectForKey(
    RSString_withValue("AuditEncodeSameID_1.0")
  )
  expect_equal(encoder$class_store$count(), 1)
  expect_true(map$objects()$containsObject(first))
  expect_true(map$objects()$containsObject(second))
})

test_that("RSClassEncoder uses generator metadata for its version key", {
  Versioned <- RSObjectClass(
    "AuditVersionDrift", version = "1.0",
    public = list(classVersion = function() "2.0")
  )
  first <- Versioned$new()
  second <- Versioned$new()
  encoder <- RSClassEncoder$instance$refresh()
  on.exit(encoder$refresh(), add = TRUE)

  encoder$encodeObject(first)
  encoder$encodeObject(second)
  map <- encoder$class_store$objectForKey(
    RSString_withValue("AuditVersionDrift_1.0")
  )
  expect_equal(encoder$class_store$count(), 1L)
  expect_equal(map$objects()$count(), 2L)
  expect_true(map$objects()$containsObject(first))
  expect_true(map$objects()$containsObject(second))
})

test_that("RSClassEncoder traverses dictionaries with class and protocol values", {
  protocol <- RSProtocolClass("EncodingDictionaryProtocol")
  dictionary <- RSMutableDictionary$new()
  dictionary$setObjectForKey(RSBoolean, RSString_withValue("class"))
  dictionary$setObjectForKey(protocol, RSString_withValue("protocol"))
  child <- RSNumber_withValue(7L)
  dictionary$setObjectForKey(child, RSString_withValue("object"))
  encoder <- RSClassEncoder$instance$refresh()
  on.exit(encoder$refresh(), add = TRUE)

  expect_error(encoder$encodeObject(dictionary), NA)
  child_map <- encoder$class_store$objectForKey(
    RSString_withValue(paste0(RSNumber$classname, "_", RSNumber$robjc_isa$version))
  )
  expect_true(child_map$objects()$containsObject(child))
  expect_error(encoder$lockInstance(), NA)
})

test_that("RSClassEncoder traverses array instances and skips generators", {
  protocol <- RSProtocolClass("EncodingArrayProtocol")
  encoder <- RSClassEncoder$instance$refresh()
  on.exit(encoder$refresh(), add = TRUE)

  for (generator in list(RSMutableArray, RSArray)) {
    child <- RSString_withValue("nested")
    array <- generator$new(list(child, RSBoolean, protocol))
    expect_identical(array$objectsForEncoding(), list(child))
    expect_error(encoder$encodeObject(array), NA)
    child_map <- encoder$class_store$objectForKey(
      RSString_withValue(paste0(RSString$classname, "_", RSString$robjc_isa$version))
    )
    expect_true(child_map$objects()$containsObject(child))
    expect_null(encoder$class_store$objectForKey(
      RSString_withValue(paste0(RSBoolean$classname, "_", RSBoolean$robjc_isa$version))
    ))
    encoder$refresh()
  }
})

test_that("RSClassEncoder clears failed work and can retry cyclic objects", {
  Earlier <- RSObjectClass("ValidationEncodeEarlier", version = "1.0")
  Child <- RSObjectClass("ValidationEncodeChild", version = "1.0")
  Parent <- RSObjectClass(
    "ValidationEncodeParent", version = "1.0",
    public = list(
      children = NULL,
      objectsForEncoding = function() self$children
    )
  )
  encoder <- RSClassEncoder$instance$refresh()
  on.exit(encoder$refresh(), add = TRUE)
  earlier <- Earlier$new()
  child <- Child$new()
  parent <- Parent$new()

  encoder$encodeObject(earlier)
  parent$children <- list(1L)
  expect_error(encoder$encodeObject(parent))
  expect_equal(encoder$class_store$count(), 0)
  expect_length(ls(encoder$visited_env, all.names = TRUE), 0L)

  parent$children <- list(child, parent)
  encoder$encodeObject(earlier)
  encoder$encodeObject(parent)
  expect_equal(encoder$class_store$count(), 3)
  for (klass in list(Earlier, Child, Parent)) {
    class_id <- RSString_withValue(paste0(klass$classname, "_1.0"))
    map <- encoder$class_store$objectForKey(class_id)
    expect_equal(map$objects()$count(), 1)
  }
})

test_that("RSClassEncoder freezes its store and can be refreshed", {
  First <- RSObjectClass("ValidationFrozenFirst", version = "1.0")
  Second <- RSObjectClass("ValidationFrozenSecond", version = "1.0")
  encoder <- RSClassEncoder$instance$refresh()
  on.exit(encoder$refresh(), add = TRUE)

  encoder$encodeObject(First$new())
  frozen <- encoder$lockInstance()
  expect_true(frozen$isMemberOf(RSDictionary))
  expect_identical(encoder$class_store, frozen)
  expect_equal(frozen$count(), 2L)
  expect_error(encoder$encodingContainer(Second$new()), "Encoder is frozen")
  expect_equal(encoder$lockedData()$count(), 2L)
  expect_warning(again <- encoder$lockInstance(), NA)
  expect_identical(again, frozen)

  encoder$refresh()
  expect_true(encoder$class_store$isMemberOf(RSMutableDictionary))
  encoder$encodeObject(Second$new())
  expect_equal(encoder$class_store$count(), 1L)
})

test_that("RSClassEncoder registers and locks superclass maps", {
  Parent <- RSObjectClass("ValidationEncoderParent", version = "1.0")
  Child <- RSObjectClass(
    "ValidationEncoderChild", super_class = Parent, version = "1.0"
  )
  child <- Child$new()
  encoder <- RSClassEncoder$instance$refresh()
  on.exit(encoder$refresh(), add = TRUE)

  encoder$encodeObject(child)
  frozen <- encoder$lockInstance()
  expect_equal(frozen$count(), 3L)
  for (klass in list(Child, Parent, RSObject)) {
    class_id <- RSString_withValue(
      paste0(klass$classname, "_", klass$robjc_isa$version)
    )
    map <- frozen$objectForKey(class_id)
    expect_true(map$isKindOf(RSInstanceMap))
    expect_error(map$addObject(klass$new()), "cannot modify after finalize")
  }
  child_map <- frozen$objectForKey(RSString_withValue("ValidationEncoderChild_1.0"))
  expect_true(child_map$objects()$containsObject(child))
})

test_that("RSClassEncoder does not replace an existing superclass class ID", {
  FirstParent <- RSObjectClass("ValidationSameParentID", version = "1.0")
  SecondParent <- RSObjectClass("ValidationSameParentID", version = "1.0")
  Child <- RSObjectClass(
    "ValidationSameParentChild", super_class = SecondParent, version = "1.0"
  )
  parent <- FirstParent$new()
  encoder <- RSClassEncoder$instance$refresh()
  on.exit(encoder$refresh(), add = TRUE)

  encoder$encodeObject(parent)
  encoder$encodeObject(Child$new())
  frozen <- encoder$lockInstance()
  map <- frozen$objectForKey(RSString_withValue("ValidationSameParentID_1.0"))
  expect_true(map$objects()$containsObject(parent))
  expect_equal(map$objects()$count(), 1L)
})
