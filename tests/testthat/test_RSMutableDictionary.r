test_that("RSMutableDictionary: set and get object for key", {
  d <- RSMutableDictionary$new()
  key <- RSString_withValue("id")
  val <- RSNumber_withValue(100)
  d$setObjectForKey(val, key)
  expect_equal(d$objectForKey(key)$value(), 100)
})

test_that("RSMutableDictionary addEntry copies a mutable key", {
  key <- RSMutableArray$new(list(RSString_withValue("original")))
  old_key <- key$copy()
  value <- RSString_withValue("value")
  dictionary <- RSMutableDictionary$new()
  dictionary$addEntry(RSKeyValuePair$new(key, value))

  key$addObject(RSString_withValue("changed"))
  expect_true(dictionary$containsKey(old_key))
  expect_identical(dictionary$objectForKey(old_key), value)
  expect_false(dictionary$containsKey(key))
  expect_true(dictionary$allKeysAsList()[[1L]]$isMemberOf(RSArray))
})

test_that("dictionary values accept RObjC class and protocol generators", {
  protocol <- RSProtocolClass("DictionaryValueProtocol")
  dictionary <- RSMutableDictionary$new()
  class_key <- RSString_withValue("class")
  protocol_key <- RSString_withValue("protocol")

  dictionary$setObjectForKey(RSString, class_key)
  dictionary$setObjectForKey(protocol, protocol_key)
  expect_identical(dictionary$objectForKey(class_key), RSString)
  expect_identical(dictionary$objectForKey(protocol_key), protocol)
  expect_identical(dictionary$allKeysForObjectAsList(RSString)[[1L]]$value(),
                   "class")
  expect_identical(dictionary$allKeysForObjectAsList(protocol)[[1L]]$value(),
                   "protocol")

  immutable <- RSDictionary_withObjectsForKeys(
    list(RSString, protocol), list(class_key, protocol_key)
  )
  expect_identical(immutable$objectForKey(class_key), RSString)
  expect_identical(immutable$objectForKey(protocol_key), protocol)
  expect_identical(dictionary$allEntriesAsList()[[1L]]$value(),
                   dictionary$allValuesAsList()[[1L]])
  expect_equal(dictionary$allValues()$count(), 2L)
  expect_equal(dictionary$allEntries()$count(), 2L)
  expect_equal(RSMutableDictionary$new()$
                 dictionaryByMergingWithDictionary(dictionary)$count(), 2L)
  expect_error(dictionary$setObjectForKey(1L, class_key),
               "Unsupported value type", fixed = TRUE)
})

test_that("dictionary supports class and protocol generator keys", {
  protocol <- RSProtocolClass("DictionaryKeyProtocol")
  dictionary <- RSMutableDictionary$new()
  dictionary$setObjectForKey(RSNumber, RSString)
  dictionary$setObjectForKey(protocol, protocol)

  expect_true(dictionary$containsKey(RSString))
  expect_identical(dictionary$objectForKey(RSString), RSNumber)
  expect_identical(dictionary$objectForKey(protocol), protocol)
  expect_true(any(vapply(dictionary$allKeysAsList(),
                         identical, logical(1), RSString)))
  expect_true(any(vapply(dictionary$allEntriesAsList(),
                         function(entry) identical(entry$key(), protocol), logical(1))))
  expect_true(dictionary$isEqual(dictionary$copy()))
  expect_identical(dictionary$allKeysForObjectAsList(RSNumber)[[1L]], RSString)

  from_pairs <- RSMutableDictionary$new(key_value_pairs = list(
    RSKeyValuePair$new(RSString, RSNumber)
  ))
  expect_identical(from_pairs$objectForKey(RSString), RSNumber)
  immutable <- RSDictionary_withObjectsForKeys(
    list(RSNumber, protocol), list(RSString, protocol)
  )
  expect_identical(immutable$objectForKey(RSString), RSNumber)
  expect_identical(immutable$objectForKey(protocol), protocol)

  dictionary$removeObjectForKey(RSString)
  expect_false(dictionary$containsKey(RSString))
  expect_identical(dictionary$count(), 1)
})

test_that("dictionary equality distinguishes object, class, and protocol values", {
  protocol <- RSProtocolClass("DictionaryEqualityValueProtocol")
  key <- RSString_withValue("key")
  make_dictionary <- function(value) {
    dictionary <- RSMutableDictionary$new()
    dictionary$setObjectForKey(value, key)
    dictionary
  }
  values <- list(RSString_withValue("x"), RSString_withValue("y"),
                 RSString, RSNumber, protocol)
  dictionaries <- lapply(values, make_dictionary)
  for (i in seq_along(dictionaries)) {
    for (j in seq_along(dictionaries)) {
      expect_identical(dictionaries[[i]]$isEqual(dictionaries[[j]]), i == j)
    }
  }
})

test_that("RSMutableDictionary validates a key hash before storage", {
  calls <- 0L
  ProbeKey <- RSObjectClass(
    "MutableDictionaryHashProbe", super_class = RSObject,
    public = list(hash = function() {
      calls <<- calls + 1L
      stop("hash failure")
    })
  )
  key <- ProbeKey$new()
  value <- RSString_withValue("value")
  dictionary <- RSMutableDictionary$new()

  expect_error(dictionary$setObjectForKey(value, key), "hash failure", fixed = TRUE)
  expect_identical(calls, 1L)
  expect_equal(dictionary$count(), 0)
  expect_error(RSDictionary$new(keys = list(key), values = list(value)),
               "hash failure", fixed = TRUE)
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

test_that("RSMutableDictionary: reverse lookup finds every equal value", {
  dictionary <- RSMutableDictionary$new()
  dictionary$setObjectForKey(RSString_withValue("same"), RSString_withValue("a"))
  dictionary$setObjectForKey(RSString_withValue("other"), RSString_withValue("b"))
  dictionary$setObjectForKey(RSString_withValue("same"), RSString_withValue("c"))

  keys <- dictionary$allKeysForObjectAsList(RSString_withValue("same"))
  expect_setequal(vapply(keys, function(key) key$value(), character(1)), c("a", "c"))
  expect_equal(dictionary$allKeysForObject(RSString_withValue("same"))$count(), 2)
  expect_length(dictionary$allKeysForObjectAsList(RSString_withValue("missing")), 0)
  expect_length(dictionary$allKeysForObjectAsList(RSNumber_withValue(1)), 0)

  dictionary$setObjectForKey(NULL, RSString_withValue("nullable"))
  null_keys <- dictionary$allKeysForObjectAsList(NULL)
  expect_equal(vapply(null_keys, function(key) key$value(), character(1)), "nullable")

  dictionary$setObjectForKey(RSString_withValue("changed"), RSString_withValue("a"))
  keys <- dictionary$allKeysForObjectAsList(RSString_withValue("same"))
  expect_equal(vapply(keys, function(key) key$value(), character(1)), "c")
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
