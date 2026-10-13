test_that("RSVector is a read-only copy of RSMutableVector", {
  mutable <- RSMutableVector$new(c(1L, 2L))
  vector <- mutable$copy()

  expect_true(vector$isMemberOf(RSVector))
  expect_identical(vector$asVector(), c(1L, 2L))
  expect_identical(vector$valueAtIndex(2L), 2L)
  expect_identical(vector$hashOperator(), mutable$hashOperator())
  expect_identical(vector$copy(), vector)

  mutable$appendValues(3L)
  expect_identical(vector$asVector(), c(1L, 2L))

  editable <- vector$mutableCopy()
  expect_true(editable$isMemberOf(RSMutableVector))
  expect_false(editable$isMemberOf(RSVector))
  editable$appendValues(4L)
  expect_identical(editable$asVector(), c(1L, 2L, 4L))
  expect_identical(vector$asVector(), c(1L, 2L))
})

test_that("RSVector rejects every inherited mutation method", {
  vector <- RSVector$new(c(1L, 2L))
  message <- msg_immutable_try_mutate(RSVector)

  expect_error(vector$appendValues(3L), message, fixed = TRUE)
  expect_error(vector$insertValuesAtIndex(3L, 1L), message, fixed = TRUE)
  expect_error(vector$replaceValuesAtIndices(3L, 1L), message, fixed = TRUE)
  expect_error(vector$removeValuesAtIndices(1L), message, fixed = TRUE)
  expect_error(vector$removeAllValues(), message, fixed = TRUE)
  expect_identical(vector$asVector(), c(1L, 2L))
})

test_that("RSVector inherits value comparison and hashing", {
  first <- RSVector$new(c(1L, NA_integer_))
  same <- RSVector$new(c(1L, NA_integer_))
  later <- RSVector$new(c(2L, NA_integer_))

  expect_true(first$isEqual(same))
  expect_false(first$isEqual(later))
  expect_identical(first$compare(later), -1L)
  expect_identical(first$hash(), same$hash())
})
