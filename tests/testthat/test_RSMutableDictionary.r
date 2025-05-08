test_that("RSMutableDictionary: set and get object for key", {
  d <- RSMutableDictionary$new()
  key <- RSString$stringWith("id")
  val <- RSNumber$numberWith(100)
  d$setObjectForKey(val, key)
  expect_equal(d$objectForKey(key)$value(), 100)
})

test_that("RSMutableDictionary: getObject returns NULL if not found", {
  d <- RSMutableDictionary$new()
  expect_null(d$objectForKey(RSString$stringWith("missing")))
})

test_that("RSMutableDictionary: overwrite value for same key", {
  d <- RSMutableDictionary$new()
  key <- RSString$stringWith("k")
  d$setObjectForKey(RSString$stringWith("v1"), key)
  d$setObjectForKey(RSString$stringWith("v2"), key)
  expect_equal(d$count(), 1)
  expect_equal(d$objectForKey(key)$value(), "v2")
})

test_that("RSMutableDictionary: removeObjectForKey deletes entry", {
  d <- RSMutableDictionary$new()
  key <- RSString$stringWith("t")
  d$setObjectForKey(RSString$stringWith("to be deleted"), key)
  d$removeObjectForKey(key)
  expect_null(d$objectForKey(key))
  expect_equal(d$count(), 0)
})

test_that("RSMutableDictionary: allKeysAsList and allValuesAsList", {
  d <- RSMutableDictionary$new()
  d$setObjectForKey(RSString$stringWith("val1"), RSString$stringWith("k1"))
  d$setObjectForKey(RSString$stringWith("val2"), RSString$stringWith("k2"))
  keys <- vapply(d$allKeysAsList(), function(x) x$value(), character(1))
  vals <- vapply(d$allValuesAsList(), function(x) x$value(), character(1))
  expect_setequal(keys, c("k1", "k2"))
  expect_setequal(vals, c("val1", "val2"))
})

test_that("RSMutableDictionary: removeAllObjects clears dictionary", {
  d <- RSMutableDictionary$new()
  d$setObjectForKey(RSString$stringWith("v"), RSString$stringWith("k"))
  d$removeAllObjects()
  expect_equal(d$count(), 0)
})

test_that("RSMutableDictionary: copyWithEnv returns RSDictionary", {
  d <- RSMutableDictionary$new()
  d$setObjectForKey(RSString$stringWith("v"), RSString$stringWith("k"))
  copied <- d$copyWithEnv()
  expect_s3_class(copied, "RSDictionary")
  expect_equal(copied$count(), 1)
})

test_that("RSMutableDictionary: mutableCopyWithEnv returns new object", {
  d <- RSMutableDictionary$new()
  d$setObjectForKey(RSString$stringWith("v"), RSString$stringWith("k"))
  copied <- d$mutableCopyWithEnv(new.env())
  expect_s3_class(copied, "RSMutableDictionary")
  expect_equal(copied$count(), 1)
  expect_false(identical(d, copied))
})

test_that("RSMutableDictionary: union and difference with another dictionary", {
  d1 <- RSMutableDictionary$new()
  d2 <- RSMutableDictionary$new()
  d1$setObjectForKey(RSString$stringWith("1"), RSString$stringWith("a"))
  d1$setObjectForKey(RSString$stringWith("2"), RSString$stringWith("b"))
  d2$setObjectForKey(RSString$stringWith("3"), RSString$stringWith("b"))
  d2$setObjectForKey(RSString$stringWith("4"), RSString$stringWith("c"))
  u <- d1$dictionaryByMergingWithDictionary(d2)
  expect_s3_class(u, "RSDictionary")
  expect_equal(u$count(), 3)
  diff <- d1$dictionaryByExcludingWithDictionary(d2)
  expect_s3_class(diff, "RSDictionary")
  expect_equal(diff$count(), 1)
  expect_equal(diff$objectForKey(RSString$stringWith("a"))$value(), "1")
})
