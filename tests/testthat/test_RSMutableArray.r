test_that("RSMutableArray: addObject and count work", {
  ary <- RSMutableArray$new()
  expect_equal(ary$count(), 0)
  ary$addObject(RSString_withValue("apple"))
  ary$addObject(RSString_withValue("banana"))
  expect_equal(ary$count(), 2)
})

test_that("array search treats an indeterminate isEqual result as no match", {
  IndeterminateEqual <- RSObjectClass(
    "ArrayIndeterminateEqual",
    super_class = RSObject,
    public = list(isEqual = function(other) NA)
  )
  needle <- RSObject$new()
  array <- RSMutableArray$new(list(
    IndeterminateEqual$new(), needle
  ))

  expect_identical(array$indexOfObject(needle), 2L)
  expect_true(array$containsObject(needle))
  expect_identical(array$indexOfObject(RSObject$new()), NA_integer_)
  expect_false(array$containsObject(RSObject$new()))
})

test_that("arrays discard input list names before storing elements", {
  item <- RSString_withValue("x")
  for (array in list(
    RSMutableArray$new(objects = list(first = item)),
    RSArray$new(objects = list(first = item)),
    RSArray_withObjects(list(first = item))
  )) {
    expect_null(names(array$asList()))
    expect_identical(array$indexOfObject(item), 1L)
    expect_null(names(array$mutableCopy()$asList()))
    expect_null(names(array$copy()$asList()))
  }
})

test_that("arrays validate the supplied hash operator at construction", {
  PartialOperator <- RSObjectClass(
    "ArrayPartialHashOperator",
    public = list(hashFromObjects = function(objects) 0L)
  )
  for (operator in list(NULL, list(), RSObject$new(), PartialOperator$new())) {
    expect_error(
      RSMutableArray$new(hash_operator = operator),
      "must provide `hashFromObjects()` and `hashesOfEachObject()`",
      fixed = TRUE
    )
    expect_error(
      RSArray$new(hash_operator = operator),
      "must provide `hashFromObjects()` and `hashesOfEachObject()`",
      fixed = TRUE
    )
  }
  array <- RSMutableArray$new(list(RSNumber_withValue(1L)))
  expect_identical(array$hashOperator(), RSHashOperator$instance)
  expect_identical(array$hashesOfObjects(), 49L)
  expect_identical(array$copy()$hashOperator(), RSHashOperator$instance)
})

test_that("array equality includes the hash operator instance", {
  ConstantOperator <- RSObjectClass(
    "ArrayEqualityHashOperator",
    public = list(
      hashFromObjects = function(objects) 1L,
      hashesOfEachObject = function(objects) rep.int(1L, length(objects))
    )
  )
  operator <- ConstantOperator$new()
  item <- RSString_withValue("a")
  standard <- RSArray$new(list(item))
  custom <- RSMutableArray$new(list(item), hash_operator = operator)
  same_operator <- RSArray$new(list(item), hash_operator = operator)

  expect_false(standard$isEqual(custom))
  expect_false(custom$isEqual(standard))
  expect_true(custom$isEqual(same_operator))
  expect_true(custom$isEqual(custom$copy()))
  expect_identical(custom$hash(), same_operator$hash())
  expect_false(RSMutableSet$new(list(standard))$containsObject(custom))
})

test_that("RSMutableArray rejects non-object mutations without changing contents", {
  ary <- RSMutableArray$new(list(RSString_withValue("keep")))
  expect_error(ary$addObject(1L), "Element must be an RObjC object", fixed = TRUE)
  expect_error(ary$insertObjectAtIndex(NULL, 1L), "Element must be an RObjC object", fixed = TRUE)
  expect_error(ary$replaceObjectAtIndex("bad", 1L), "Element must be an RObjC object", fixed = TRUE)
  expect_identical(ary$count(), 1L)
  expect_identical(ary$objectAtIndex(1L)$value(), "keep")
})

test_that("RSMutableArray accepts class and protocol elements", {
  protocol <- RSProtocolClass("ArrayElementProtocol")
  ary <- RSMutableArray$new(list(RSString, protocol))
  expect_identical(ary$objectAtIndex(1L), RSString)
  expect_identical(ary$objectAtIndex(2L), protocol)
  ary$addObject(RSNumber)
  ary$insertObjectAtIndex(RSBoolean, 1L)
  ary$replaceObjectAtIndex(protocol, 3L)
  expect_identical(ary$asList(), list(RSBoolean, RSString, protocol, RSNumber))
  expect_identical(ary$copy()$asList(), ary$asList())
  expect_identical(ary$indexOfObject(RSString), 2L)
  expect_true(ary$containsObject(protocol))
  expect_true(ary$isEqual(ary$copy()))
  expect_type(ary$hash(), "integer")
})

test_that("RSMutableArray rejects invalid indices before reading or mutating", {
  ary <- RSMutableArray$new(list(
    RSString_withValue("a"), RSString_withValue("b")
  ))
  item <- RSString_withValue("x")
  for (index in list(1.9, NA_real_, Inf, c(1, 2), "1", 0, 4)) {
    expect_error(ary$objectAtIndex(index), "Index out of bounds or not a whole number", fixed = TRUE)
    expect_error(ary$insertObjectAtIndex(item, index), "Index out of bounds or not a whole number", fixed = TRUE)
    expect_error(ary$replaceObjectAtIndex(item, index), "Index out of bounds or not a whole number", fixed = TRUE)
    expect_error(ary$removeObjectAtIndex(index), "Index out of bounds or not a whole number", fixed = TRUE)
  }
  expect_identical(vapply(ary$asList(), function(x) x$value(), character(1)), c("a", "b"))
  expect_identical(ary$objectAtIndex(1.0)$value(), "a")
  ary$insertObjectAtIndex(item, 3.0)
  expect_identical(vapply(ary$asList(), function(x) x$value(), character(1)), c("a", "b", "x"))
})

test_that("RSMutableArray: objectAtIndex returns correct element", {
  ary <- RSMutableArray$new()
  ary$addObject(RSString_withValue("a"))
  ary$addObject(RSString_withValue("b"))
  expect_equal(ary$objectAtIndex(2)$value(), "b")
  expect_equal(ary$objectAtIndex(1)$value(), "a")
})

test_that("RSMutableArray: index and membership searches handle missing objects", {
  ary <- RSMutableArray$new(objects = list(
    RSString_withValue("a"), RSString_withValue("b")
  ))
  expect_identical(ary$indexOfObject(RSString_withValue("b")), 2L)
  expect_true(ary$containsObject(RSString_withValue("a")))
  expect_true(ary$containsObject(RSString_withValue("b")))
  expect_identical(ary$indexOfObject(RSString_withValue("missing")), NA_integer_)
  expect_false(ary$containsObject(RSString_withValue("missing")))

  empty <- RSMutableArray$new()
  expect_identical(empty$indexOfObject(RSString_withValue("a")), NA_integer_)
  expect_false(empty$containsObject(RSString_withValue("a")))
})

test_that("RSMutableArray equality compares contents across mutability", {
  immutable <- RSArray$new(list(RSString_withValue("a")))
  mutable <- immutable$mutableCopy()
  expect_true(immutable$isEqual(mutable))
  expect_true(mutable$isEqual(immutable))
  expect_identical(immutable$hash(), mutable$hash())
  expect_true(RSArray$new()$isEqual(RSMutableArray$new()))
  expect_false(immutable$isEqual(RSArray$new()))
  expect_false(immutable$isEqual(RSArray$new(list(RSString_withValue("b")))))
  expect_false(immutable$isEqual(1L))
  expect_false(immutable$isEqual(NULL))
})

test_that("RSMutableArray: insertObjectAtIndex works", {
  ary <- RSMutableArray$new()
  ary$addObject(RSString_withValue("a"))
  ary$addObject(RSString_withValue("c"))
  ary$insertObjectAtIndex(RSString_withValue("b"), 2)
  values <- vapply(ary$allObjectsAsList(), function(x) x$value(), character(1))
  expect_equal(values, c("a", "b", "c"))
})

test_that("RSMutableArray: replaceObjectAtIndex works", {
  ary <- RSMutableArray$new()
  ary$addObject(RSString_withValue("X"))
  ary$replaceObjectAtIndex(RSString_withValue("Y"), 1)
  expect_equal(ary$objectAtIndex(1)$value(), "Y")
})

test_that("RSMutableArray: removeObjectAtIndex removes correct item", {
  ary <- RSMutableArray$new()
  ary$addObject(RSString_withValue("a"))
  ary$addObject(RSString_withValue("b"))
  ary$addObject(RSString_withValue("c"))
  ary$removeObjectAtIndex(2)
  values <- vapply(ary$allObjectsAsList(), function(x) x$value(), character(1))
  expect_equal(values, c("a", "c"))
})

test_that("RSMutableArray: removeAllObjects resets array", {
  ary <- RSMutableArray$new()
  ary$addObject(RSString_withValue("a"))
  ary$addObject(RSString_withValue("b"))
  ary$removeAllObjects()
  expect_equal(ary$count(), 0)
})

test_that("RSMutableArray compare follows lexicographic element order", {
  string <- function(value) RSString_withValue(value)
  ab <- RSArray$new(list(string("a"), string("b")))
  ba <- RSMutableArray$new(list(string("b"), string("a")))
  expect_identical(ab$compare(ba), -1L)
  expect_identical(ba$compare(ab), 1L)
  expect_identical(ab$compare(ab$mutableCopy()), 0L)
  expect_identical(RSArray$new(list(string("z")))$compare(
    RSArray$new(list(string("aa")))), 1L)
  expect_identical(RSArray$new(list(string("a")))$compare(ab), -1L)
  expect_identical(ab$compare(RSArray$new(list(string("a")))), 1L)
  expect_identical(RSArray$new()$compare(RSMutableArray$new()), 0L)
})

test_that("RSMutableArray compare validates inputs and forwards NA ordering", {
  missing <- RSArray$new(list(RSString$na))
  present <- RSArray$new(list(RSString_withValue("a")))
  expect_identical(missing$compare(present), 1L)
  expect_identical(missing$compare(present, na.last = FALSE), -1L)
  expect_identical(missing$compare(present, na.last = 1L), 1L)
  expect_error(missing$compare(present, na.last = NA), "`na.last` must be TRUE or FALSE", fixed = TRUE)
  expect_error(missing$compare(1L), "Cannot compare objects of different class", fixed = TRUE)
})
