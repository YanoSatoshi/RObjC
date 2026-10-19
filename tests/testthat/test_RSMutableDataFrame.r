test_that("DataFrame equality handles valid and incompatible inputs", {
  mutable <- RSMutableDataFrame$new(data.frame(x = 1L))
  same <- RSMutableDataFrame$new(data.frame(x = 1L))
  different <- RSMutableDataFrame$new(data.frame(x = 2L))
  immutable <- RSDataFrame$new(data.frame(x = 1L))

  expect_true(mutable$isEqual(mutable))
  expect_true(mutable$isEqual(same))
  expect_true(mutable$isEqual(immutable))
  expect_true(immutable$isEqual(mutable))
  expect_false(mutable$isEqual(different))
  for (other in list(NULL, 1L, list(), "x", RSString_withValue("x"))) {
    expect_false(mutable$isEqual(other))
    expect_false(immutable$isEqual(other))
  }
})
