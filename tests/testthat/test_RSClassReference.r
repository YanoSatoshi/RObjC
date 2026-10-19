test_that("class reference rejects values that are not wrapped classes", {
  for (value in list(NULL, 1L, list(), RSString_withValue("x"))) {
    expect_error(
      RSClassReference$new(value),
      "`class_ref` must be an RSWrappedClass object.",
      fixed = TRUE
    )
  }
})
