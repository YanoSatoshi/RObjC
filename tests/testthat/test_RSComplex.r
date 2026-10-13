test_that("RSComplex wraps complex values and dispatches from RSScalar", {
  value <- 1 + 2i
  wrapped <- RSComplex_withValue(value)
  expect_true(wrapped$isMemberOf(RSComplex))
  expect_identical(wrapped$value(), value)
  expect_true(RSScalar_withValue(value)$isMemberOf(RSComplex))
  expect_identical(RSComplex_withValue(NA_complex_), RSComplex$na)
  expect_identical(RSScalar_withValue(NA_complex_), RSComplex$na)
  expect_identical(RSComplex$na$class(), RSNAComplex)
  expect_identical(RSComplex$na$value(), NA_complex_)
})

test_that("RSComplex compares and hashes complex scalars", {
  first <- RSComplex$new(1 + 2i)
  same <- RSComplex$new(1 + 2i)
  later <- RSComplex$new(1 + 3i)
  expect_true(first$isEqual(same))
  expect_false(first$isEqual(later))
  expect_identical(first$compare(later), -1L)
  expect_identical(first$hash(), same$hash())
  expect_identical(RSComplex$na$compare(first), 1L)
  expect_identical(RSComplex$na$compare(first, na.last = FALSE), -1L)
  expect_type(RSComplex$na$hash(), "integer")
})

test_that("RSComplex rejects other types and non-scalars", {
  expect_error(RSComplex$new(1), "complex scalar", fixed = TRUE)
  expect_error(RSComplex_withValue(c(1 + 2i, 3 + 4i)),
               "complex scalar", fixed = TRUE)
  expect_error(RSComplex$new(NA_complex_), "RSNAComplex", fixed = TRUE)
})
