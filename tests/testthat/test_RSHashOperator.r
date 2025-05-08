test_that("RSHashOperator: computeHashCodeWithString is stable", {
  op <- RSHashOperator$module()
  h1 <- op$computeHashCodeWithString("apple")
  h2 <- op$computeHashCodeWithString("apple")
  h3 <- op$computeHashCodeWithString("banana")
  expect_equal(h1, h2)
  expect_type(h1, "integer")
  expect_false(h1 == h3)
})

test_that("RSHashOperator: hashesOfEachObject returns correct hashes", {
  nums <- lapply(1:3, RSNumber$numberWith)
  op <- RSHashOperator$module()
  hashes <- op$hashesOfEachObject(nums)
  expect_type(hashes, "integer")
  expect_length(hashes, 3)
  expect_true(all(hashes > 0))
})

test_that("RSHashOperator: areEqualSets detects equality correctly", {
  a <- lapply(c("a", "b", "c"), RSString$stringWith)
  b <- lapply(c("c", "b", "a"), RSString$stringWith)
  c <- lapply(c("a", "b", "d"), RSString$stringWith)
  op <- RSHashOperator$module()
  expect_true(op$areEqualSets(a, b))
  expect_false(op$areEqualSets(a, c))
})

test_that("RSHashOperator: objectsNotIn works as expected", {
  a <- lapply(c("a", "b", "c"), RSString$stringWith)
  b <- lapply(c("b", "c"), RSString$stringWith)
  result <- RSHashOperator$module()$objectsNotIn(a, b)
  expect_length(result, 1)
  expect_equal(result[[1]]$value(), "a")
})

test_that("RSHashOperator: objectsAlsoIn filters common elements", {
  a <- lapply(c("x", "y", "z"), RSString$stringWith)
  b <- lapply(c("y", "z", "w"), RSString$stringWith)
  common <- RSHashOperator$module()$objectsAlsoIn(a, b)
  expect_length(common, 2)
  values <- vapply(common, function(obj) obj$value(), character(1))
  expect_setequal(values, c("y", "z"))
})

test_that("RSHashOperator: unionOfSets combines without duplicates", {
  a <- lapply(c("a", "b"), RSString$stringWith)
  b <- lapply(c("b", "c"), RSString$stringWith)
  unioned <- RSHashOperator$module()$unionOfSets(a, b)
  values <- vapply(unioned, function(obj) obj$value(), character(1))
  expect_setequal(values, c("a", "b", "c"))
})

test_that("RSHashOperator: differenceOfSets returns unique part", {
  a <- lapply(c("a", "b", "c"), RSString$stringWith)
  b <- lapply(c("b", "c"), RSString$stringWith)
  diff <- RSHashOperator$module()$differenceOfSets(a, b)
  expect_length(diff, 1)
  expect_equal(diff[[1]]$value(), "a")
})
