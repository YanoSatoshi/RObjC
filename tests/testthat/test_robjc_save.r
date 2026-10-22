test_that("robjc_save writes the object and class metadata", {
  load_dummies()
  on.exit(unload_dummies(), add = TRUE)
  file <- tempfile(fileext = ".RData")
  on.exit(unlink(file), add = TRUE)
  source_env <- new.env(parent = emptyenv())
  source_env$obj <- DummyClass$new()

  robjc_save(list = "obj", envir = source_env, file = file)

  restored_env <- new.env(parent = emptyenv())
  base::load(file, envir = restored_env)
  expect_true(file.exists(file))
  expect_true(".__rs_encoded_data__" %in% ls(restored_env, all.names = TRUE))
  expect_equal(restored_env$obj$class()$classname, "DummyClass")
  expect_equal(restored_env$obj$class()$robjc_isa$version, "1.2")
})

test_that("robjc_load resolves classes and omits internal metadata", {
  file <- tempfile(fileext = ".RData")
  on.exit(unlink(file), add = TRUE)
  source_env <- new.env(parent = emptyenv())
  source_env$item <- RSString_withValue("saved")
  robjc_save(list = "item", envir = source_env, file = file)

  restored_env <- new.env(parent = emptyenv())
  loaded_names <- robjc_load(file, envir = restored_env)
  expect_identical(loaded_names, "item")
  expect_false(exists(".__rs_encoded_data__", envir = restored_env, inherits = FALSE))
  expect_identical(restored_env$item$class(), RSString)
  expect_identical(restored_env$item$value(), "saved")
})

test_that("robjc_save rejects the reserved metadata name before writing", {
  file <- tempfile(fileext = ".RData")
  on.exit(unlink(file), add = TRUE)
  source_env <- new.env(parent = emptyenv())
  source_env$.__rs_encoded_data__ <- "user value"

  expect_error(
    robjc_save(list = ".__rs_encoded_data__", envir = source_env, file = file),
    "`.__rs_encoded_data__` is reserved for robjc metadata.", fixed = TRUE
  )
  expect_false(file.exists(file))
  expect_identical(source_env$.__rs_encoded_data__, "user value")
})
