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
