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

test_that("RSHashOperator: scalar hashing rejects lists and dimensions", {
  op <- RSHashOperator$instance

  expect_error(op$hashForScalar(matrix(1, 1, 1)), "`val` must be a scalar.", fixed = TRUE)
  expect_error(op$hashForScalar(list(1)), "`val` must be a scalar.", fixed = TRUE)
  expect_identical(op$hashForScalar(1L), op$hashForScalar(1))
  expect_type(op$hashForScalar(as.POSIXlt("2020-01-01", tz = "UTC")), "integer")
})

test_that("RSHashOperator: hashesOfEachObject returns correct hashes", {
  nums <- lapply(1:3, RSNumber_withValue)
  op <- RSHashOperator$instance
  hashes <- op$hashesOfEachObject(nums)
  expect_type(hashes, "integer")
  expect_length(hashes, 3)
  expect_true(all(hashes > 0))
})

test_that("RSHashOperator: aggregate XOR cannot return the NA integer sentinel", {
  HighHash <- RSObjectClass(
    "AuditHighXorHash",
    public = list(hash = function() -2147418112L)
  )
  LowHash <- RSObjectClass(
    "AuditLowXorHash",
    public = list(hash = function() 65536L)
  )
  high <- HighHash$new()
  low <- LowHash$new()
  op <- RSHashOperator$instance

  expect_identical(op$hashFromObjects(list(high)), -2147418112L)
  expect_identical(op$hashFromObjects(list(high, low)), 0L)
})

test_that("RSHashOperator: aggregate hash rejects a missing component", {
  MissingHash <- RSObjectClass(
    "AuditMissingComponentHash",
    public = list(hash = function() NA_integer_)
  )

  expect_error(
    RSHashOperator$instance$hashFromObjects(list(MissingHash$new())),
    "Object hashes must be non-missing integers.", fixed = TRUE
  )
})

test_that("RSHashOperator: set operations reject missing object hashes", {
  MissingHash <- RSObjectClass(
    "AuditMissingSetOperationHash",
    public = list(hash = function() NA_integer_)
  )
  bad <- MissingHash$new()
  good <- RSString_withValue("good")
  op <- RSHashOperator$instance
  message <- "Object hashes must be non-missing integers."

  expect_error(op$hashesOfEachObject(list(bad)), message, fixed = TRUE)
  expect_error(op$areEqualSets(list(bad), list(bad)), message, fixed = TRUE)
  expect_error(op$objectsAlsoIn(list(bad), list(good)), message, fixed = TRUE)
  expect_error(op$objectsAlsoIn(list(good), list(bad)), message, fixed = TRUE)
  expect_error(op$objectsNotIn(list(bad), list(good)), message, fixed = TRUE)
  expect_error(op$objectsNotIn(list(good), list(bad)), message, fixed = TRUE)
  expect_error(op$unionOfSets(list(bad), list(good)), message, fixed = TRUE)
  expect_error(op$intersectionOfSets(list(bad), list(good)), message, fixed = TRUE)
  expect_error(op$differenceOfSets(list(bad), list(good)), message, fixed = TRUE)
})

test_that("RSHashOperator: areEqualSets detects equality correctly", {
  a <- lapply(c("a", "b", "c"), RSString_withValue)
  b <- lapply(c("c", "b", "a"), RSString_withValue)
  c <- lapply(c("a", "b", "d"), RSString_withValue)
  op <- RSHashOperator$instance
  expect_true(op$areEqualSets(a, b))
  expect_false(op$areEqualSets(a, c))
})

test_that("RSHashOperator: set equality ignores list names", {
  first <- RSString_withValue("a")
  second <- RSString_withValue("b")
  op <- RSHashOperator$instance

  expect_true(op$areEqualSets(
    list(left = first, right = second),
    list(second = second, first = first)
  ))
})

test_that("RSHashOperator: objectsNotIn works as expected", {
  a <- lapply(c("a", "b", "c"), RSString_withValue)
  b <- lapply(c("b", "c"), RSString_withValue)
  result <- RSHashOperator$instance$objectsNotIn(a, b)
  expect_length(result, 1)
  expect_equal(result[[1]]$value(), "a")
})

test_that("RSHashOperator: indeterminate equality does not discard objects", {
  IndeterminateEqual <- RSObjectClass(
    "HashOperatorIndeterminateEqual",
    super_class = RSObject,
    public = list(
      hash = function() 1L,
      isEqual = function(other) NA
    )
  )
  left <- IndeterminateEqual$new()
  right <- IndeterminateEqual$new()
  op <- RSHashOperator$instance

  expect_false(op$areEqualSets(list(left), list(right)))
  expect_length(op$objectsAlsoIn(list(left), list(right)), 0L)
  expect_identical(op$objectsNotIn(list(left), list(right)), list(left))
  expect_identical(op$unionOfSets(list(left), list(right)), list(right, left))
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
