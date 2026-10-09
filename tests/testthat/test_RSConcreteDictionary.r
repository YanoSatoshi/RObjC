test_that("RSConcreteDictionary stores and retrieves objects", {
  storage <- RSConcreteDictionary$new()
  key <- RSString_withValue("key")
  value <- RSString_withValue("value")

  expect_equal(storage$count(), 0)
  expect_equal(storage$contentHash(), 0L)

  expect_identical(storage$set(key, value), storage)
  expect_true(storage$contains(RSString_withValue("key")))
  expect_identical(storage$get(RSString_withValue("key")), value)
  expect_equal(storage$count(), 1)
  expect_equal(storage$contentHash(), key$hash())
})

test_that("RSConcreteDictionary enumerates corresponding keys and values", {
  storage <- RSConcreteDictionary$new()
  storage$set(RSString_withValue("k1"), RSString_withValue("v1"))
  storage$set(RSString_withValue("k2"), RSString_withValue("v2"))

  keys <- storage$keys()
  values <- storage$values()

  expect_length(keys, 2)
  expect_length(values, 2)
  expect_true(all(vapply(
    seq_along(keys),
    function(i) identical(storage$get(keys[[i]]), values[[i]]),
    logical(1)
  )))
})

test_that("RSConcreteDictionary duplicate has independent storage", {
  storage <- RSConcreteDictionary$new()
  storage$set(RSString_withValue("key"), RSString_withValue("original"))

  copied <- storage$duplicate()
  storage$set(RSString_withValue("key"), RSString_withValue("changed"))

  expect_equal(copied$get(RSString_withValue("key"))$value(), "original")
  expect_equal(storage$get(RSString_withValue("key"))$value(), "changed")
})

test_that("RSConcreteDictionary removes and clears entries", {
  storage <- RSConcreteDictionary$new()
  storage$set(RSString_withValue("k1"), RSString_withValue("v1"))
  storage$set(RSString_withValue("k2"), RSString_withValue("v2"))

  expect_true(storage$remove(RSString_withValue("k1")))
  expect_false(storage$remove(RSString_withValue("missing")))
  expect_equal(storage$count(), 1)

  expect_identical(storage$clear(), storage)
  expect_equal(storage$count(), 0)
  expect_length(storage$keys(), 0)
  expect_length(storage$values(), 0)
})
