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
  expect_equal(restored_env$dummy$classVersion(), "1.2")
})
