test_that("robjc_save correctly saves object with robjc metadata", {
  # Load dummy class definition
  load_dummies()
  # Create dummy object and assign to environment
  obj <- DummyClass$new()
  file <- tempfile(fileext = ".RData")
  # Save the object using robjc_save()
  robjc_save(obj, file = file)
  # Confirm file creation
  expect_true(file.exists(file))
  # Load saved data into isolated environment
  temp_env <- new.env()
  base::load(file = file, envir = temp_env)
  # Confirm metadata exists
  expect_true(".__rs_encoded_data__" %in% ls(envir = temp_env, all.names = TRUE))
  metadata <- temp_env$.__rs_encoded_data__
  # Decode the metadata using RSClassDecoder
  decoder <- RSClassDecoder$module()
  decoder$loadEncodedData(metadata)
  # Create class map for resolution
  class_map <- RSMutableDictionary$new()
  class_map$setObjectForKey(
    RSWrappedClass$new(DummyClass),
    RSString$stringWith("DummyClass_1.2")
  )
  decoder$resolveClassesWithDictionary(class_map)
  # Unpack into new environment and validate
  unpacked_env <- new.env()
  decoder$unpackTo(unpacked_env)
  restored <- get(ls(unpacked_env), envir = unpacked_env)
  # Check class identity via robjc OOP system
  expect_true(restored$isKindOf(DummyClass))
  expect_equal(restored$class()$classname, obj$class()$classname)
  # Clean up global env
  unload_dummies()
})
