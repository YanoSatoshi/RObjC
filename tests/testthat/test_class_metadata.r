test_that("exported RObjC generators have class versions and identity records", {
  ns <- asNamespace("robjc")
  exported <- getNamespaceExports("robjc")
  classes <- lapply(exported, get, envir = ns)
  classes <- Filter(robjc_isMetaClass, classes)

  expect_gt(length(classes), 0)
  for (klass in classes) {
    expect_identical(klass$robjc_isa$class, klass)
    expect_identical(klass$private_fields$.__isa, klass$robjc_isa)
    expect_type(klass$robjc_isa$version, "character")
    expect_length(klass$robjc_isa$version, 1)
  }
})
