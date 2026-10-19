test_that("RSClassDecoder reads class IDs from current encoder metadata", {
  load_dummies()
  on.exit(unload_dummies(), add = TRUE)
  encoder <- RSClassEncoder$instance$refresh()
  encoder$encodeObject(DummyClass$new())
  decoder <- RSClassDecoder$instance$loadEncodedData(encoder$class_store)

  expect_identical(decoder$encodedClassIDs(), "DummyClass_1.2")
})

test_that("RSClassDecoder reports missing class mappings", {
  load_dummies()
  on.exit(unload_dummies(), add = TRUE)
  encoder <- RSClassEncoder$instance$refresh()
  encoder$encodeObject(DummyClass$new())
  decoder <- RSClassDecoder$instance$loadEncodedData(encoder$class_store)

  expect_error(
    decoder$resolveClassesWithDictionary(RSMutableDictionary$new()),
    "Unresolved classIDs: DummyClass_1.2",
    fixed = TRUE
  )
})

test_that("RSClassDecoder skips unresolved ancestry entries with no instances", {
  Parent <- RSObjectClass("DecodeEmptyParent", version = "1.0")
  SavedChild <- RSObjectClass(
    "DecodeEmptyChild", super_class = Parent, version = "1.0"
  )
  CurrentChild <- RSObjectClass(
    "DecodeEmptyChild", super_class = Parent, version = "1.0"
  )
  child <- SavedChild$new()
  encoder <- RSClassEncoder$instance$refresh()
  on.exit(encoder$refresh(), add = TRUE)
  encoder$encodeObject(child)
  decoder <- RSClassDecoder$instance$loadEncodedData(encoder$lockInstance())
  class_map <- RSMutableDictionary$new()
  class_map$setObjectForKey(
    RSWrappedClass$new(CurrentChild),
    RSString_withValue("DecodeEmptyChild_1.0")
  )

  expect_error(decoder$resolveClassesWithDictionary(class_map), NA)
  expect_identical(child$class(), CurrentChild)
  expect_identical(decoder$unresolved, character())
})

test_that("RSClassDecoder rejects non-dictionary class maps", {
  decoder <- RSClassDecoder$instance$loadEncodedData(RSMutableDictionary$new())
  for (invalid in list(NULL, 1L, list(), RSString_withValue("x"))) {
    expect_error(
      decoder$resolveClassesWithDictionary(invalid),
      "`class_map` must be a dictionary.", fixed = TRUE
    )
  }
})

test_that("RSClassDecoder rejects fake encoded dictionaries without changing state", {
  valid <- RSMutableDictionary$new()
  decoder <- RSClassDecoder$instance$loadEncodedData(valid)

  expect_error(
    decoder$loadEncodedData(structure(list(), class = "RSMutableDictionary")),
    "`encoded_data` must be a dictionary.", fixed = TRUE
  )
  expect_identical(decoder$class_store, valid)
  expect_identical(decoder$encodedClasses(), list())
})

test_that("RSClassDecoder validates the unpack target even with no instances", {
  decoder <- RSClassDecoder$instance$loadEncodedData(RSDictionary$new())
  for (invalid in list(NULL, 1L, list())) {
    expect_error(decoder$unpackTo(invalid),
                 "`envir` must be an environment.", fixed = TRUE)
  }
  target <- new.env(parent = emptyenv())
  expect_identical(decoder$unpackTo(target), decoder)

  encoder <- RSClassEncoder$instance$refresh()
  on.exit(encoder$refresh(), add = TRUE)
  object <- RSString_withValue("decoded")
  encoder$encodeObject(object)
  decoder$loadEncodedData(encoder$lockInstance())
  expect_error(decoder$unpackTo(1L),
               "`envir` must be an environment.", fixed = TRUE)
  expect_identical(decoder$unpackTo(target), decoder)
  expect_identical(get(object$objectID(), envir = target), object)
})

test_that("RSClassDecoder does not partially resolve missing class mappings", {
  First <- RSObjectClass("ValidationDecodeFirst", version = "1.0")
  Second <- RSObjectClass("ValidationDecodeSecond", version = "1.0")
  encoder <- RSClassEncoder$instance$refresh()
  encoder$encodeObject(First$new())
  encoder$encodeObject(Second$new())
  entries <- encoder$class_store$allEntriesAsList()
  first_object <- entries[[1L]]$value()$objects()$anyObject()
  second_object <- entries[[2L]]$value()$objects()$anyObject()
  first_class <- first_object$class()
  second_class <- second_object$class()
  replacement <- RSObjectClass(
    first_class$classname,
    version = first_class$robjc_isa$version
  )

  class_map <- RSMutableDictionary$new()
  class_map$setObjectForKey(RSWrappedClass$new(replacement), entries[[1L]]$key())
  decoder <- RSClassDecoder$instance$loadEncodedData(encoder$class_store)
  on.exit({
    encoder$refresh()
    decoder$loadEncodedData(RSMutableDictionary$new())
  }, add = TRUE)

  expect_error(
    decoder$resolveClassesWithDictionary(class_map),
    paste0("Unresolved classIDs: ", entries[[2L]]$key()$value()),
    fixed = TRUE
  )
  expect_identical(first_object$class(), first_class)
  expect_identical(second_object$class(), second_class)

  class_map$setObjectForKey(RSString_withValue("invalid"), entries[[2L]]$key())
  expect_error(
    decoder$resolveClassesWithDictionary(class_map),
    "`class_map` must map class IDs to matching RSWrappedClass objects.",
    fixed = TRUE
  )
  expect_identical(first_object$class(), first_class)
  expect_identical(second_object$class(), second_class)

  class_map$setObjectForKey(RSWrappedClass$new(second_class), entries[[2L]]$key())
  class_map$setObjectForKey(RSWrappedClass$new(second_class), entries[[1L]]$key())
  expect_error(
    decoder$resolveClassesWithDictionary(class_map),
    "`class_map` must map class IDs to matching RSWrappedClass objects.",
    fixed = TRUE
  )
  expect_identical(first_object$class(), first_class)
  expect_identical(second_object$class(), second_class)

  class_map$setObjectForKey(RSWrappedClass$new(replacement), entries[[1L]]$key())
  expect_identical(decoder$resolveClassesWithDictionary(class_map), decoder)
  expect_identical(first_object$class(), replacement)
  expect_identical(second_object$class(), second_class)
  expect_identical(decoder$unresolved, character())
})
