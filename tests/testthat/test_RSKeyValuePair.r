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

test_that("RSKeyValuePair: rejects non-RSObject key or value", {
  bad_key <- "str"
  good_val <- RSNumber_withValue(1)
  expect_error(RSKeyValuePair$new(bad_key, good_val), "Key is not kind of RSObject.")
  good_key <- RSString_withValue("ok")
  bad_val <- 123
  expect_error(RSKeyValuePair$new(good_key, bad_val), "Value is not kind of RSObject.")
})
