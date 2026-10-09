test_that("RSHashOperator: computeHashCodeWithString is stable", {
  op <- RSHashOperator$instance
  h1 <- op$computeHashCodeWithString("apple")
  h2 <- op$computeHashCodeWithString("apple")
  h3 <- op$computeHashCodeWithString("banana")
  expect_equal(h1, h2)
  expect_type(h1, "integer")
  expect_false(h1 == h3)
})

test_that("RSHashOperator: scalar hashes are well distributed", {
  op <- RSHashOperator$instance
  values <- sprintf("key-%08d", seq_len(1000))
  hashes <- vapply(values, op$hashForScalar, integer(1))

  expect_false(anyNA(hashes))
  expect_equal(length(unique(hashes)), length(values))
})

test_that("RSHashOperator: equal numeric scalars have equal hashes", {
  op <- RSHashOperator$instance

  expect_equal(op$hashForScalar(1L), op$hashForScalar(1))
  expect_type(op$hashForScalar(NA_real_), "integer")
  expect_false(is.na(op$hashForScalar(NA_real_)))
})

test_that("RSHashOperator: hashesOfEachObject returns correct hashes", {
  nums <- lapply(1:3, RSNumber_withValue)
  op <- RSHashOperator$instance
  hashes <- op$hashesOfEachObject(nums)
  expect_type(hashes, "integer")
  expect_length(hashes, 3)
  expect_true(all(hashes > 0))
})

test_that("RSHashOperator: areEqualSets detects equality correctly", {
  a <- lapply(c("a", "b", "c"), RSString_withValue)
  b <- lapply(c("c", "b", "a"), RSString_withValue)
  c <- lapply(c("a", "b", "d"), RSString_withValue)
  op <- RSHashOperator$instance
  expect_true(op$areEqualSets(a, b))
  expect_false(op$areEqualSets(a, c))
})

test_that("RSHashOperator: objectsNotIn works as expected", {
  a <- lapply(c("a", "b", "c"), RSString_withValue)
  b <- lapply(c("b", "c"), RSString_withValue)
  result <- RSHashOperator$instance$objectsNotIn(a, b)
  expect_length(result, 1)
  expect_equal(result[[1]]$value(), "a")
})

test_that("RSHashOperator: objectsAlsoIn filters common elements", {
  a <- lapply(c("x", "y", "z"), RSString_withValue)
  b <- lapply(c("y", "z", "w"), RSString_withValue)
  common <- RSHashOperator$instance$objectsAlsoIn(a, b)
  expect_length(common, 2)
  values <- vapply(common, function(obj) obj$value(), character(1))
  expect_setequal(values, c("y", "z"))
})

test_that("RSHashOperator: unionOfSets combines without duplicates", {
  a <- lapply(c("a", "b"), RSString_withValue)
  b <- lapply(c("b", "c"), RSString_withValue)
  unioned <- RSHashOperator$instance$unionOfSets(a, b)
  values <- vapply(unioned, function(obj) obj$value(), character(1))
  expect_setequal(values, c("a", "b", "c"))
})

test_that("RSHashOperator: differenceOfSets returns unique part", {
  a <- lapply(c("a", "b", "c"), RSString_withValue)
  b <- lapply(c("b", "c"), RSString_withValue)
  diff <- RSHashOperator$instance$differenceOfSets(a, b)
  expect_length(diff, 1)
  expect_equal(diff[[1]]$value(), "a")
})
