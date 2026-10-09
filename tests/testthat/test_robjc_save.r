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
  expect_equal(restored_env$obj$classVersion(), "1.2")
})
