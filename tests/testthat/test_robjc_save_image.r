test_that("robjc_save_image correctly saves .GlobalEnv objects with robjc metadata", {
  # Load dummy class
  load_dummies()
  # Create test object and assign into global environment
  dummy <- DummyClass$new()
  assign("dummy", dummy, envir = .GlobalEnv)
  file <- tempfile(fileext = ".RData")
  # Save all global objects using robjc_save_image
  robjc_save_image(file = file)
  expect_true(file.exists(file))
  # Load into new environment
  temp_env <- new.env()
  base::load(file = file, envir = temp_env)
  # Confirm metadata presence
  expect_true(".__rs_encoded_data__" %in% ls(temp_env, all.names = TRUE))
  metadata <- temp_env$.__rs_encoded_data__
  # Decode
  decoder <- RSClassDecoder$module()
  decoder$loadEncodedData(metadata)
  # Class map
  class_map <- RSMutableDictionary$new()
  class_map$setObjectForKey(
    RSWrappedClass$new(DummyClass),
    RSString$stringWith("DummyClass_1.2")
  )
  decoder$resolveClassesWithDictionary(class_map)
  # Unpack and locate object by class
  unpacked_env <- new.env()
  decoder$unpackTo(unpacked_env)
  # Get restored object by searching for the DummyClass instance
  restored <- Filter(function(x) x$isKindOf(DummyClass), mget(ls(unpacked_env), unpacked_env))[[1]]
  expect_true(restored$isKindOf(DummyClass))
  expect_equal(restored$class()$classname, dummy$class()$classname)
  # Clean up
  rm(dummy, envir = .GlobalEnv)
  unload_dummies()
})
