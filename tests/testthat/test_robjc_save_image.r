test_that("robjc_save_image uses its supplied environment", {
  load_dummies()
  on.exit(unload_dummies(), add = TRUE)
  file <- tempfile(fileext = ".RData")
  on.exit(unlink(file), add = TRUE)
  source_env <- new.env(parent = emptyenv())
  source_env$dummy <- DummyClass$new()

  robjc_save_image(file = file, envir = source_env)

  restored_env <- new.env(parent = emptyenv())
  loaded_names <- base::load(file, envir = restored_env)
  expect_setequal(loaded_names, c("dummy", ".__rs_encoded_data__"))
  expect_equal(restored_env$dummy$class()$classname, "DummyClass")
  expect_equal(restored_env$dummy$class()$robjc_isa$version, "1.2")
})

test_that("robjc_save_image rejects the reserved metadata name before writing", {
  file <- tempfile(fileext = ".RData")
  on.exit(unlink(file), add = TRUE)
  source_env <- new.env(parent = emptyenv())
  source_env$.__rs_encoded_data__ <- "user value"
  source_env$item <- RSString_withValue("x")

  expect_error(
    robjc_save_image(file = file, envir = source_env),
    "`.__rs_encoded_data__` is reserved for robjc metadata.", fixed = TRUE
  )
  expect_false(file.exists(file))
  expect_identical(source_env$.__rs_encoded_data__, "user value")
})
