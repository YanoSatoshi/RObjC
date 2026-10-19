test_that("RSKeyValuePair: can be created with RSObject key and value", {
  key <- RSString_withValue("id")
  value <- RSNumber_withValue(42)
  pair <- RSKeyValuePair$new(key, value)
  expect_s3_class(pair, "RSKeyValuePair")
  expect_equal(pair$key()$value(), "id")
  expect_equal(pair$value()$value(), 42)
})

test_that("RSKeyValuePair: isEqual compares by key only", {
  key1 <- RSString_withValue("k")
  key2 <- RSString_withValue("k")
  val1 <- RSString_withValue("v1")
  val2 <- RSString_withValue("v2")
  p1 <- RSKeyValuePair$new(key1, val1)
  p2 <- RSKeyValuePair$new(key2, val2)
  expect_true(p1$isEqual(p2))  # keys are equal
})

test_that("RSKeyValuePair: hash returns key's hash", {
  key <- RSString_withValue("alpha")
  val <- RSString_withValue("v")
  pair <- RSKeyValuePair$new(key, val)
  expect_equal(pair$hash(), key$hash())
})

test_that("RSKeyValuePair: rejects unsupported key or value", {
  bad_key <- "str"
  good_val <- RSNumber_withValue(1)
  expect_error(RSKeyValuePair$new(bad_key, good_val),
               "Key must be an RObjC object, class, or protocol.")
  good_key <- RSString_withValue("ok")
  bad_val <- 123
  expect_error(RSKeyValuePair$new(good_key, bad_val),
               "Value must be an RObjC object, class, or protocol.")
})

test_that("RSKeyValuePair accepts class and protocol keys and values", {
  key <- RSString_withValue("key")
  protocol <- RSProtocolClass("KeyValuePairValueProtocol")
  expect_identical(RSKeyValuePair$new(key, RSString)$value(), RSString)
  expect_identical(RSKeyValuePair$new(key, protocol)$value(), protocol)
  class_pair <- RSKeyValuePair$new(RSString, RSString)
  protocol_pair <- RSKeyValuePair$new(protocol, key)
  expect_identical(class_pair$key(), RSString)
  expect_identical(class_pair$hash(), RSString$hash())
  expect_true(class_pair$isEqual(RSKeyValuePair$new(RSString, key)))
  expect_identical(protocol_pair$key(), protocol)
})

test_that("RSKeyValuePair exposes only RObjC instances to the encoder", {
  key <- RSString_withValue("key")
  value <- RSNumber_withValue(7L)
  protocol <- RSProtocolClass("EncodingPairProtocol")

  expect_identical(
    RSKeyValuePair$new(key, value)$objectsForEncoding(),
    list(key, value)
  )
  expect_identical(
    RSKeyValuePair$new(key, RSString)$objectsForEncoding(),
    list(key)
  )
  expect_identical(
    RSKeyValuePair$new(key, protocol)$objectsForEncoding(),
    list(key)
  )
  expect_identical(
    RSKeyValuePair$new(RSString, value)$objectsForEncoding(),
    list(value)
  )
  expect_identical(
    RSKeyValuePair$new(protocol, RSString)$objectsForEncoding(),
    list()
  )
})

test_that("RSKeyValuePair: setKey rejects invalid keys without changing state", {
  original <- RSString_withValue("original")
  pair <- RSKeyValuePair$new(original, RSNull$null)
  expect_error(pair$setKey(1L),
               "Key must be an RObjC object, class, or protocol.", fixed = TRUE)
  expect_identical(pair$key(), original)
  expect_identical(pair$hash(), original$hash())
})

test_that("RSKeyValuePair: isEqual returns FALSE for other types", {
  pair <- RSKeyValuePair$new(RSString_withValue("k"), RSNull$null)
  for (other in list(NULL, 1L, list(), "x", RSString_withValue("k"))) {
    expect_false(pair$isEqual(other))
  }
})
