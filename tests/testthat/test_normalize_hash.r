test_that("robjc_normalize_hash maps only a missing hash to zero", {
  expect_identical(robjc_normalize_hash(NA_integer_), 0L)
  expect_identical(robjc_normalize_hash(42L), 42L)
  expect_identical(robjc_normalize_hash(-42L), -42L)
})

test_that("an array normalizes the final hash from a custom operator", {
  MissingAggregate <- RSObjectClass(
    "MissingAggregateHashOperator",
    public = list(
      hashFromObjects = function(objects) NA_integer_,
      hashesOfEachObject = function(objects) integer()
    )
  )
  array <- RSMutableArray$new(hash_operator = MissingAggregate$new())

  expect_identical(array$hash(), 0L)
})
