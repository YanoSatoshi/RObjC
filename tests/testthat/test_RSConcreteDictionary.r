test_that("RSConcreteDictionary stores and retrieves objects", {
  storage <- RSConcreteDictionary$new()
  key <- RSString$stringWith("key")
  value <- RSString$stringWith("value")

  expect_equal(storage$count(), 0)
  expect_equal(storage$contentHash(), 0L)

  expect_identical(storage$set(key, value), storage)
  expect_true(storage$contains(RSString$stringWith("key")))
  expect_identical(storage$get(RSString$stringWith("key")), value)
  expect_equal(storage$count(), 1)
  expect_equal(storage$contentHash(), key$hash())
})

test_that("RSConcreteDictionary enumerates corresponding keys and values", {
  storage <- RSConcreteDictionary$new()
  storage$set(RSString$stringWith("k1"), RSString$stringWith("v1"))
  storage$set(RSString$stringWith("k2"), RSString$stringWith("v2"))

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
  storage$set(RSString$stringWith("key"), RSString$stringWith("original"))

  copied <- storage$duplicate()
  storage$set(RSString$stringWith("key"), RSString$stringWith("changed"))

  expect_equal(copied$get(RSString$stringWith("key"))$value(), "original")
  expect_equal(storage$get(RSString$stringWith("key"))$value(), "changed")
})

test_that("RSConcreteDictionary removes and clears entries", {
  storage <- RSConcreteDictionary$new()
  storage$set(RSString$stringWith("k1"), RSString$stringWith("v1"))
  storage$set(RSString$stringWith("k2"), RSString$stringWith("v2"))

  expect_true(storage$remove(RSString$stringWith("k1")))
  expect_false(storage$remove(RSString$stringWith("missing")))
  expect_equal(storage$count(), 1)

  expect_identical(storage$clear(), storage)
  expect_equal(storage$count(), 0)
  expect_length(storage$keys(), 0)
  expect_length(storage$values(), 0)
})
