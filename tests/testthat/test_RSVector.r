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

test_that("RSMutableVector: aggregate XOR cannot return the NA integer sentinel", {
  XorOperator <- RSObjectClass(
    "AuditVectorXorOperator",
    public = list(hashForScalar = function(val) {
      if (identical(val, 1L)) -2147418112L else 65536L
    })
  )
  operator <- XorOperator$new()

  expect_identical(RSMutableVector$new(1L, hash_operator = operator)$hash(),
                   -2147418112L)
  expect_identical(RSMutableVector$new(1:2, hash_operator = operator)$hash(),
                   0L)
})

test_that("RSMutableVector: hash operator cannot return a missing component", {
  MissingOperator <- RSObjectClass(
    "AuditMissingVectorHashOperator",
    public = list(hashForScalar = function(val) NA_integer_)
  )
  vector <- RSMutableVector$new(1L, hash_operator = MissingOperator$new())

  expect_error(vector$hashesOfValues(),
               "`hashForScalar()` must return non-missing integers.", fixed = TRUE)
  expect_error(vector$hash(),
               "`hashForScalar()` must return non-missing integers.", fixed = TRUE)
})

test_that("vector equality includes the hash operator instance", {
  ConstantOperator <- RSObjectClass(
    "VectorEqualityHashOperator",
    public = list(hashForScalar = function(val) 1L)
  )
  operator <- ConstantOperator$new()
  standard <- RSVector$new(1L)
  custom <- RSMutableVector$new(1L, hash_operator = operator)
  same_operator <- RSVector$new(1L, hash_operator = operator)

  expect_false(standard$isEqual(custom))
  expect_false(custom$isEqual(standard))
  expect_true(custom$isEqual(same_operator))
  expect_true(custom$isEqual(custom$copy()))
  expect_identical(custom$hash(), same_operator$hash())
  expect_false(RSMutableSet$new(list(standard))$containsObject(custom))
})

test_that("mutable and immutable vector copies compare by value", {
  mutable <- RSMutableVector$new(c(1L, 2L))
  immutable <- mutable$copy()

  expect_true(mutable$isEqual(immutable))
  expect_true(immutable$isEqual(mutable))
  expect_identical(mutable$compare(immutable), 0L)
  expect_identical(immutable$compare(mutable), 0L)
  expect_identical(mutable$hash(), immutable$hash())

  mutable$appendValues(3L)
  expect_false(mutable$isEqual(immutable))
  expect_false(immutable$isEqual(mutable))
  expect_identical(mutable$compare(immutable), 1L)
  expect_identical(immutable$compare(mutable), -1L)
})

test_that("vector comparison coerces na.last consistently", {
  for (generator in list(RSMutableVector, RSVector)) {
    missing <- generator$new(c(NA_real_))
    present <- generator$new(c(1))
    expect_identical(missing$compare(present, na.last = 1L), 1L)
    expect_identical(missing$compare(present, na.last = 0L), -1L)
    expect_identical(present$compare(missing, na.last = 1L), -1L)
    for (invalid in list(NA, c(TRUE, FALSE), "invalid", list())) {
      expect_error(missing$compare(present, na.last = invalid),
                   "`na.last` must be TRUE or FALSE.", fixed = TRUE)
    }
  }
})

test_that("factor vector comparison rejects different level orders", {
  a <- RSMutableVector$new(factor("a", levels = c("a", "b"), ordered = TRUE))
  b <- RSVector$new(factor("b", levels = c("b", "a"), ordered = TRUE))
  same_levels <- RSVector$new(factor("b", levels = c("a", "b"), ordered = TRUE))

  expect_error(a$compare(b), "Factor level sets differ.", fixed = TRUE)
  expect_error(b$compare(a), "Factor level sets differ.", fixed = TRUE)
  expect_identical(a$compare(same_levels), -1L)
  expect_identical(same_levels$compare(a), 1L)
})

test_that("factor and non-factor vectors have no shared sorting order", {
  factor_vector <- RSMutableVector$new(factor("z", levels = c("z", "a")))
  character_vector <- RSVector$new("a")
  integer_vector <- RSVector$new(1L)

  expect_error(factor_vector$compare(character_vector),
               "Factor and non-factor vectors have no shared ordering.", fixed = TRUE)
  expect_error(character_vector$compare(factor_vector),
               "Factor and non-factor vectors have no shared ordering.", fixed = TRUE)
  expect_error(factor_vector$compare(integer_vector),
               "Factor and non-factor vectors have no shared ordering.", fixed = TRUE)
  expect_error(integer_vector$compare(factor_vector),
               "Factor and non-factor vectors have no shared ordering.", fixed = TRUE)
})

test_that("vector compare rejects all different stored value types", {
  integer <- RSVector$new(2L)
  double <- RSMutableVector$new(2)
  character <- RSVector$new("10")

  expect_error(integer$compare(character),
               "Vector value types are not comparable.", fixed = TRUE)
  expect_error(character$compare(integer),
               "Vector value types are not comparable.", fixed = TRUE)
  expect_error(integer$compare(double),
               "Vector value types are not comparable.", fixed = TRUE)
  expect_error(double$compare(integer),
               "Vector value types are not comparable.", fixed = TRUE)
  expect_identical(integer$compare(RSMutableVector$new(3L)), -1L)
  expect_identical(integer$nativeCompare(character), 1L)
  expect_identical(integer$nativeCompare(double), 0L)

  unordered <- RSVector$new(factor("a", levels = c("a", "b")))
  ordered <- RSVector$new(ordered("a", levels = c("a", "b")))
  expect_error(unordered$compare(ordered),
               "Vector value types are not comparable.", fixed = TRUE)
})

test_that("raw vectors cannot be ordered even when empty", {
  for (generator in list(RSMutableVector, RSVector)) {
    empty <- generator$new(raw())
    one <- generator$new(as.raw(1))
    two <- generator$new(as.raw(2))
    for (pair in list(list(empty, empty), list(empty, one),
                      list(one, empty), list(one, two))) {
      expect_error(pair[[1L]]$compare(pair[[2L]]),
                   "Raw vectors cannot be sorted by base R.", fixed = TRUE)
    }
    expect_identical(one$nativeCompare(two), -1L)
  }
})

test_that("nativeCompare follows base R atomic operators", {
  number <- RSMutableVector$new(c(1L, 2L))
  character <- RSVector$new(c("1", "3"))
  expect_identical(number$nativeCompare(character), -1L)
  expect_identical(character$nativeCompare(number), 1L)
  expect_identical(number$nativeCompare(RSVector$new(c(1, 2))), 0L)
  expect_identical(number$nativeCompare(RSVector$new(c(1L, 2L, 0L))), -1L)
  expect_identical(RSVector$new(integer())$nativeCompare(
    RSMutableVector$new(integer())), 0L)

  ordered_factor <- RSVector$new(ordered("z", levels = c("z", "a")))
  string <- RSMutableVector$new("a")
  expect_identical(ordered_factor$nativeCompare(string), -1L)
  expect_identical(string$nativeCompare(ordered_factor), 1L)
  expect_warning(
    expect_error(RSVector$new(factor("z"))$nativeCompare(string),
                 "Native values have no ordering.", fixed = TRUE),
    "not meaningful for factors"
  )

  expect_identical(RSVector$new(1 + 2i)$nativeCompare(
    RSMutableVector$new(1 + 2i)), 0L)
  expect_error(RSVector$new(1 + 2i)$nativeCompare(
    RSMutableVector$new(1 + 3i)), "invalid comparison with complex values")
})

test_that("nativeCompare places missing values and validates arguments", {
  missing <- RSMutableVector$new(c(1, NA_real_))
  present <- RSVector$new(c(1, 2))
  expect_identical(missing$nativeCompare(present), 1L)
  expect_identical(missing$nativeCompare(present, na.last = FALSE), -1L)
  expect_identical(RSVector$new(NaN)$nativeCompare(
    RSMutableVector$new(NA_real_)), 0L)
  expect_error(missing$nativeCompare(present, na.last = NA), "`na.last`")
  expect_error(missing$nativeCompare(1), "vector object")
})
