test_that("DataFrame equality handles valid and incompatible inputs", {
  mutable <- RSMutableDataFrame$new(data.frame(x = 1L))
  same <- RSMutableDataFrame$new(data.frame(x = 1L))
  different <- RSMutableDataFrame$new(data.frame(x = 2L))
  immutable <- RSDataFrame$new(data.frame(x = 1L))

  expect_true(mutable$isKindOf(RSDataFrame))
  expect_false(immutable$isKindOf(RSMutableDataFrame))
  expect_true(mutable$respondsTo("setData"))
  expect_true(mutable$respondsTo("setColumn"))
  expect_false(immutable$respondsTo("setData"))
  expect_false(immutable$respondsTo("setColumn"))
  expect_identical(immutable$copy(), immutable)
  expect_identical(mutable$copy()$class(), RSDataFrame)
  expect_identical(immutable$mutableCopy()$class(), RSMutableDataFrame)

  expect_true(mutable$isEqual(mutable))
  expect_true(mutable$isEqual(same))
  expect_true(mutable$isEqual(immutable))
  expect_true(immutable$isEqual(mutable))
  expect_false(mutable$isEqual(different))
  expect_false(RSMutableDataFrame$new(data.frame(x = 1))$isEqual(
    RSMutableDataFrame$new(data.frame(x = 1 + 1e-9))
  ))
  for (other in list(NULL, 1L, list(), "x", RSString_withValue("x"))) {
    expect_false(mutable$isEqual(other))
    expect_false(immutable$isEqual(other))
  }
})

test_that("DataFrame hash uses row count, column names, and storage types", {
  first <- RSMutableDataFrame$new(tibble::tibble(
    id = 1L, payload = list(list(value = 1L))
  ))
  second <- RSDataFrame$new(tibble::tibble(
    id = 2L, payload = list(list(value = 2L))
  ))
  same <- first$copy()

  expect_true(first$isEqual(same))
  expect_identical(first$hash(), same$hash())
  expect_false(first$isEqual(second))
  expect_identical(first$hash(), second$hash())
  expect_identical(first$hash(), first$mutableCopy()$hash())
  expect_type(first$hash(), "integer")
  expect_false(is.na(first$hash()))

  data <- first$getData()
  seed <- c(nrow(data), paste(names(data), vapply(data, typeof, character(1)), sep = "/"))
  expected <- robjc_normalize_hash(Reduce(bitwXor,
    vapply(seed, RSHashOperator$instance$computeHashCodeWithString, integer(1)),
    init = 0L
  ))
  expect_identical(first$hash(), expected)
})

test_that("data frame wrappers work across mutable and immutable collection lookups", {
  immutable <- RSDataFrame$new(data.frame(id = 1L))
  mutable <- RSMutableDataFrame$new(data.frame(id = 1L))
  set <- RSSet$new(list(immutable))
  dictionary <- RSDictionary$new(
    keys = list(immutable), values = list(RSString$new("found"))
  )

  expect_true(set$containsObject(mutable))
  expect_identical(dictionary$objectForKey(mutable)$value(), "found")
})
