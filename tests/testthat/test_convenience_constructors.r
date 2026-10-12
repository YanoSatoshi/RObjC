test_that("array constructors preserve order and copy mutable storage", {
  first <- RSString_withValue("first")
  second <- RSString_withValue("second")

  expect_equal(RSArray_empty()$count(), 0)
  expect_equal(RSMutableArray_empty()$count(), 0)
  expect_equal(RSArray_withObject(first)$objectAtIndex(1)$value(), "first")
  expect_equal(RSMutableArray_withObject(first)$objectAtIndex(1)$value(), "first")

  for (constructor in list(RSArray_withObjects, RSMutableArray_withObjects)) {
    array <- constructor(list(first, second))
    expect_identical(
      vapply(array$asList(), function(object) object$value(), character(1)),
      c("first", "second")
    )
  }

  source <- RSMutableArray_withObjects(list(first))
  immutable <- RSArray_withArray(source)
  mutable <- RSMutableArray_withArray(source)
  source$addObject(second)
  mutable$addObject(RSString_withValue("third"))

  expect_true(immutable$isMemberOf(RSArray))
  expect_true(mutable$isMemberOf(RSMutableArray))
  expect_equal(immutable$count(), 1)
  expect_equal(mutable$count(), 2)
  expect_equal(source$count(), 2)
  expect_identical(RSArray_withArray(immutable), immutable)
})

test_that("array constructors reject invalid inputs and unsupported capacity", {
  expect_error(RSArray_withObject("x"), "list of RObjC objects", fixed = TRUE)
  expect_error(RSMutableArray_withObjects(list(RSString_withValue("x"), "y")),
               "list of RObjC objects", fixed = TRUE)
  expect_error(RSArray_withObjects(RSString_withValue("x")),
               "list of RObjC objects", fixed = TRUE)
  expect_error(RSArray_withArray(list()), "RSArray or RSMutableArray", fixed = TRUE)
  expect_error(RSMutableArray_withArray(RSSet_empty()),
               "RSArray or RSMutableArray", fixed = TRUE)
  expect_error(RSMutableArray_withCapacity(-1), "non-negative whole number", fixed = TRUE)
  expect_error(RSMutableArray_withCapacity(1.5), "non-negative whole number", fixed = TRUE)
  expect_error(RSMutableArray_withCapacity(NA_integer_), "non-negative whole number", fixed = TRUE)
  expect_error(RSMutableArray_withCapacity(0), "not implemented", fixed = TRUE)
})

test_that("set constructors deduplicate and copy mutable storage", {
  a <- RSString_withValue("a")
  b <- RSString_withValue("b")

  expect_equal(RSSet_empty()$count(), 0)
  expect_equal(RSMutableSet_empty()$count(), 0)
  expect_equal(RSSet_withObject(a)$count(), 1)
  expect_equal(RSMutableSet_withObject(a)$count(), 1)

  array <- RSArray_withObjects(list(a, a, b))
  for (constructor in list(RSSet_withObjects, RSMutableSet_withObjects)) {
    expect_equal(constructor(list(a, a, b))$count(), 2)
  }
  for (constructor in list(RSSet_withArray, RSMutableSet_withArray)) {
    set <- constructor(array)
    expect_equal(set$count(), 2)
    expect_true(set$containsObject(RSString_withValue("a")))
  }

  source <- RSMutableSet_withObject(a)
  immutable <- RSSet_withSet(source)
  mutable <- RSMutableSet_withSet(source)
  source$addObject(b)
  mutable$addObject(RSString_withValue("c"))

  expect_true(immutable$isMemberOf(RSSet))
  expect_true(mutable$isMemberOf(RSMutableSet))
  expect_equal(immutable$count(), 1)
  expect_equal(mutable$count(), 2)
  expect_equal(source$count(), 2)
  expect_identical(RSSet_withSet(immutable), immutable)
})

test_that("set constructors reject invalid collections", {
  expect_error(RSSet_withObject("a"), "list of RObjC objects", fixed = TRUE)
  expect_error(RSMutableSet_withObjects(list(RSString_withValue("a"), "b")),
               "list of RObjC objects", fixed = TRUE)
  expect_error(RSSet_withArray(list()), "RSArray or RSMutableArray", fixed = TRUE)
  expect_error(RSMutableSet_withSet(RSArray_empty()),
               "RSSet or RSMutableSet", fixed = TRUE)
})

test_that("dictionary constructors preserve mappings and isolate copies", {
  key <- RSString_withValue("key")
  value <- RSString_withValue("value")

  expect_equal(RSDictionary_empty()$count(), 0)
  expect_equal(RSMutableDictionary_empty()$count(), 0)

  constructors <- list(
    function() RSDictionary_withObjectForKey(value, key),
    function() RSMutableDictionary_withObjectForKey(value, key),
    function() RSDictionary_withObjectsForKeys(list(value), list(key)),
    function() RSMutableDictionary_withObjectsForKeys(list(value), list(key)),
    function() RSDictionary_withObjectsAndKeys(list(value, key)),
    function() RSMutableDictionary_withObjectsAndKeys(list(value, key))
  )
  for (constructor in constructors) {
    dictionary <- constructor()
    expect_equal(dictionary$count(), 1)
    expect_equal(dictionary$objectForKey(RSString_withValue("key"))$value(), "value")
  }

  source <- RSMutableDictionary_withObjectForKey(value, key)
  immutable <- RSDictionary_withDictionary(source)
  mutable <- RSMutableDictionary_withDictionary(source)
  source$setObjectForKey(RSString_withValue("changed"), key)
  mutable$removeObjectForKey(key)

  expect_true(immutable$isMemberOf(RSDictionary))
  expect_true(mutable$isMemberOf(RSMutableDictionary))
  expect_equal(immutable$objectForKey(key)$value(), "value")
  expect_false(mutable$containsKey(key))
  expect_equal(source$objectForKey(key)$value(), "changed")
  expect_identical(RSDictionary_withDictionary(immutable), immutable)
})

test_that("dictionary constructors validate keys, values, and pair shapes", {
  key <- RSString_withValue("key")
  value <- RSString_withValue("value")

  expect_error(RSDictionary_withObjectForKey("value", key),
               "values must be RObjC objects", fixed = TRUE)
  expect_error(RSDictionary_withObjectForKey(value, "key"),
               "keys must be RObjC objects conforming to RSCopying", fixed = TRUE)
  expect_error(RSMutableDictionary_withObjectsForKeys(list(value), list()),
               "same length", fixed = TRUE)
  expect_error(RSDictionary_withObjectsForKeys(list(value), "key"),
               "must be lists", fixed = TRUE)
  expect_error(RSDictionary_withObjectsAndKeys(list(value)),
               "even-length list", fixed = TRUE)
  expect_error(RSMutableDictionary_withObjectsAndKeys(list(value, "key")),
               "keys must be RObjC objects conforming to RSCopying", fixed = TRUE)
  expect_error(RSDictionary_withDictionary(RSArray_empty()),
               "RSDictionary or RSMutableDictionary", fixed = TRUE)
})

test_that("remaining scalar constructors return typed values", {
  before_date <- Sys.Date()
  before_time <- Sys.time()
  today <- RSDate_today()$value()
  now <- RSDateTime_now()$value()
  after_time <- Sys.time()
  after_date <- Sys.Date()

  expect_identical(RSNull_null(), RSNull$null)
  expect_equal(RSString_empty()$value(), "")
  expect_equal(RSString_withString("text")$value(), "text")
  expect_equal(RSString_withString(RSString_withValue("text"))$value(), "text")
  expect_identical(RSNumber_withInteger(1L)$value(), 1L)
  expect_identical(RSNumber_withDouble(1.5)$value(), 1.5)
  expect_true(today %in% c(before_date, after_date))
  expect_true(now >= before_time && now <= after_time)
  expect_error(RSString_withString(1L), "character scalar or RSString", fixed = TRUE)
})
