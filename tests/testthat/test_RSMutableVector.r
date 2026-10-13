test_that("RSMutableVector preserves its value type", {
  vector <- RSMutableVector$new(c(1L, 2L))
  vector$appendValues(3L)
  vector$insertValuesAtIndex(4L, 2L)
  vector$replaceValuesAtIndices(c(5L, 6L), c(1L, 3L))
  expect_identical(vector$asVector(), c(5L, 4L, 6L, 3L))
  expect_identical(vector$containsValues(c(4L, 9L)), c(TRUE, FALSE))
  expect_identical(vector$indicesOfValues(c(6L, 9L)), c(3L, NA_integer_))

  for (method in list(vector$appendValues,
                      function(x) vector$insertValuesAtIndex(x, 1L),
                      function(x) vector$replaceValuesAtIndices(x, 1L),
                      vector$containsValues, vector$indicesOfValues)) {
    expect_error(method(1), "vector's type and class", fixed = TRUE)
    expect_error(method(list(1L)), "vector's type and class", fixed = TRUE)
  }
  expect_error(RSMutableVector$new(list(1L)), "atomic vector", fixed = TRUE)
  expect_error(RSMutableVector$new(matrix(1L)), "atomic vector", fixed = TRUE)
  expect_error(RSMutableVector$new(1L, hash_operator = NULL),
               "hashForScalar", fixed = TRUE)

  factor_vector <- RSMutableVector$new(factor("a", levels = c("a", "b")))
  expect_error(factor_vector$appendValues(factor("c", levels = c("a", "c"))),
               "vector's type and class", fixed = TRUE)

  dates <- RSMutableVector$new(as.Date("2024-01-01"))
  dates$appendValues(as.Date("2024-01-02"))
  expect_identical(dates$asVector(), as.Date(c("2024-01-01", "2024-01-02")))

  custom <- structure(c(1L, 2L), class = "custom_vector")
  custom_vector <- RSMutableVector$new(custom)
  expect_error(custom_vector$appendValues(structure(3L, class = "custom_vector")),
               "vector's type and class", fixed = TRUE)
  expect_identical(custom_vector$asVector(), custom)
})

test_that("RSMutableVector converts numeric indices with as.integer", {
  vector <- RSMutableVector$new(c(10L, 20L))
  vector$insertValuesAtIndex(15L, 2.9)
  expect_identical(vector$asVector(), c(10L, 15L, 20L))
  expect_identical(vector$valueAtIndex(2.9), 15L)
  vector$replaceValuesAtIndices(25L, 3.9)
  expect_identical(vector$asVector(), c(10L, 15L, 25L))
  vector$removeValuesAtIndices(2.9)
  expect_identical(vector$asVector(), c(10L, 25L))
  vector$insertValuesAtIndex(30L, 3.9)
  expect_identical(vector$asVector(), c(10L, 25L, 30L))

  for (index in list(0, -1, 4, NA_real_, Inf, NaN, "1", 2^31)) {
    expect_error(vector$valueAtIndex(index), "Index out of bounds", fixed = TRUE)
    expect_error(vector$removeValuesAtIndices(index),
                 "Index out of bounds", fixed = TRUE)
    expect_error(vector$replaceValuesAtIndices(9L, index),
                 "Index out of bounds", fixed = TRUE)
  }
  for (index in list(0, -1, 5, NA_real_, Inf, NaN, "1", 2^31)) {
    expect_error(vector$insertValuesAtIndex(9L, index),
                 "Index out of bounds", fixed = TRUE)
  }
  expect_error(vector$valueAtIndex(c(1, 2)), "Index out of bounds", fixed = TRUE)
  expect_error(vector$insertValuesAtIndex(4L, numeric()),
               "Index out of bounds", fixed = TRUE)
})

test_that("RSMutableVector validates replacement lengths and empty indices", {
  vector <- RSMutableVector$new(c(1L, 2L, 3L))
  expect_error(vector$replaceValuesAtIndices(9L, c(1, 2)),
               "same length", fixed = TRUE)
  expect_error(vector$replaceValuesAtIndices(c(9L, 8L), 1),
               "same length", fixed = TRUE)
  expect_identical(vector$asVector(), c(1L, 2L, 3L))

  vector$removeValuesAtIndices(numeric())
  vector$replaceValuesAtIndices(integer(), numeric())
  expect_identical(vector$asVector(), c(1L, 2L, 3L))
  vector$removeAllValues()
  expect_identical(vector$asVector(), integer())

  empty <- RSMutableVector$new(integer())
  empty$insertValuesAtIndex(1L, 1.9)
  expect_identical(empty$asVector(), 1L)
})
