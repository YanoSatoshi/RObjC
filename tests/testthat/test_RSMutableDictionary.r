test_that("RSMutableDictionary: set and get object for key", {
  d <- RSMutableDictionary$new()
  key <- RSString_withValue("id")
  val <- RSNumber_withValue(100)
  d$setObjectForKey(val, key)
  expect_equal(d$objectForKey(key)$value(), 100)
})

test_that("RSMutableDictionary: getObject returns NULL if not found", {
  d <- RSMutableDictionary$new()
  expect_null(d$objectForKey(RSString_withValue("missing")))
})

test_that("RSMutableDictionary: overwrite value for same key", {
  d <- RSMutableDictionary$new()
  key <- RSString_withValue("k")
  d$setObjectForKey(RSString_withValue("v1"), key)
  d$setObjectForKey(RSString_withValue("v2"), key)
  expect_equal(d$count(), 1)
  expect_equal(d$objectForKey(key)$value(), "v2")
})

test_that("RSMutableDictionary: removeObjectForKey deletes entry", {
  d <- RSMutableDictionary$new()
  key <- RSString_withValue("t")
  d$setObjectForKey(RSString_withValue("to be deleted"), key)
  d$removeObjectForKey(key)
  expect_null(d$objectForKey(key))
  expect_equal(d$count(), 0)
})

test_that("RSMutableDictionary: allKeysAsList and allValuesAsList", {
  d <- RSMutableDictionary$new()
  d$setObjectForKey(RSString_withValue("val1"), RSString_withValue("k1"))
  d$setObjectForKey(RSString_withValue("val2"), RSString_withValue("k2"))
  keys <- vapply(d$allKeysAsList(), function(x) x$value(), character(1))
  vals <- vapply(d$allValuesAsList(), function(x) x$value(), character(1))
  expect_setequal(keys, c("k1", "k2"))
  expect_setequal(vals, c("val1", "val2"))
})

test_that("RSMutableDictionary: removeAllObjects clears dictionary", {
  d <- RSMutableDictionary$new()
  d$setObjectForKey(RSString_withValue("v"), RSString_withValue("k"))
  d$removeAllObjects()
  expect_equal(d$count(), 0)
})

test_that("RSMutableDictionary: copy returns RSDictionary", {
  d <- RSMutableDictionary$new()
  d$setObjectForKey(RSString_withValue("v"), RSString_withValue("k"))
  copied <- d$copy()
  expect_equal(copied$count(), 1)
})

test_that("RSMutableDictionary: mutableCopy returns new object", {
  d <- RSMutableDictionary$new()
  d$setObjectForKey(RSString_withValue("v"), RSString_withValue("k"))
  copied <- d$mutableCopy()
  expect_equal(copied$count(), 1)
  expect_false(identical(d, copied))
})

test_that("RSMutableDictionary: union and difference with another dictionary", {
  d1 <- RSMutableDictionary$new()
  d2 <- RSMutableDictionary$new()
  d1$setObjectForKey(RSString_withValue("1"), RSString_withValue("a"))
  d1$setObjectForKey(RSString_withValue("2"), RSString_withValue("b"))
  d2$setObjectForKey(RSString_withValue("3"), RSString_withValue("b"))
  d2$setObjectForKey(RSString_withValue("4"), RSString_withValue("c"))
  u <- d1$dictionaryByMergingWithDictionary(d2)
  expect_equal(u$count(), 3)
  diff <- d1$dictionaryByExcludingWithDictionary(d2)
  expect_equal(diff$count(), 1)
  expect_equal(diff$objectForKey(RSString_withValue("a"))$value(), "1")
})

test_that("RSMutableDictionary: supported initializers use native storage", {
  key <- RSString_withValue("key")
  value <- RSString_withValue("value")
  pair <- RSKeyValuePair$new(key, value)

  from_pairs <- RSMutableDictionary$new(key_value_pairs = list(pair))
  from_alternating <- RSMutableDictionary$new(
    objects_and_keys = list(value, key)
  )
  from_separate <- RSMutableDictionary$new(keys = list(key), values = list(value))

  for (dictionary in list(from_pairs, from_alternating, from_separate)) {
    expect_equal(dictionary$count(), 1)
    expect_equal(dictionary$objectForKey(key)$value(), "value")
  }
  expect_equal(RSMutableDictionary$new(objects_and_keys = list())$count(), 0)
})

test_that("RSMutableDictionary: stored NULL differs from an absent key", {
  dictionary <- RSMutableDictionary$new()
  key <- RSString_withValue("nullable")
  missing <- RSString_withValue("missing")

  dictionary$setObjectForKey(NULL, key)

  expect_true(dictionary$containsKey(key))
  expect_identical(dictionary$objectForKey(key), RSNull$null)
  expect_false(dictionary$containsKey(missing))
  expect_null(dictionary$objectForKey(missing))
})

test_that("RSMutableDictionary: copies own independent native tables", {
  key <- RSString_withValue("key")
  original <- RSMutableDictionary$new(
    keys = list(key),
    values = list(RSString_withValue("before"))
  )

  immutable <- original$copy()
  mutable <- original$mutableCopy()
  original$setObjectForKey(RSString_withValue("after"), key)
  mutable$removeObjectForKey(key)

  expect_true(immutable$isMemberOf(RSDictionary))
  expect_equal(immutable$objectForKey(key)$value(), "before")
  expect_equal(original$objectForKey(key)$value(), "after")
  expect_false(mutable$containsKey(key))
})

test_that("RSMutableDictionary: equality compares mappings and hash uses keys", {
  key_a <- RSString_withValue("a")
  key_b <- RSString_withValue("b")
  value_1 <- RSString_withValue("1")
  value_2 <- RSString_withValue("2")

  left <- RSMutableDictionary$new(
    keys = list(key_a, key_b), values = list(value_1, value_2)
  )
  same <- RSMutableDictionary$new(
    keys = list(key_b, key_a), values = list(value_2, value_1)
  )
  different <- RSMutableDictionary$new(
    keys = list(key_a, key_b), values = list(value_2, value_1)
  )

  expect_true(left$isEqual(same))
  expect_equal(left$hash(), same$hash())
  expect_false(left$isEqual(different))
  expect_equal(left$hash(), different$hash())
})
