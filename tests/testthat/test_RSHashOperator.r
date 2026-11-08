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

test_that("RSHashOperator: hashesOfObjects returns correct hashes", {
  nums <- lapply(1:3, RSNumber_withValue)
  op <- RSHashOperator$instance
  hashes <- op$hashesOfObjects(nums)
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

  expect_identical(op$combinedHashFromObjects(list(high)), -2147418112L)
  expect_identical(op$combinedHashFromObjects(list(high, low)), 0L)
})

test_that("RSHashOperator: aggregate hash rejects a missing component", {
  MissingHash <- RSObjectClass(
    "AuditMissingComponentHash",
    public = list(hash = function() NA_integer_)
  )

  expect_error(
    RSHashOperator$instance$combinedHashFromObjects(list(MissingHash$new())),
    "Object hashes must be non-missing integers.", fixed = TRUE
  )
})

test_that("RSHashOperator: hashesOfObjects rejects a missing component", {
  MissingHash <- RSObjectClass(
    "AuditMissingObjectHash",
    public = list(hash = function() NA_integer_)
  )
  expect_error(
    RSHashOperator$instance$hashesOfObjects(list(MissingHash$new())),
    "Object hashes must be non-missing integers.", fixed = TRUE
  )
})
