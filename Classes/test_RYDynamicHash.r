library(testthat)

test_that("RSDynamicHash: setObject and getObject store and retrieve object", {
  h <- RSDynamicHash$new()
  key <- RSString$stringWith("alpha")
  h$setObject(key)
  result <- h$getObject(key)
  expect_s3_class(result, "RSString")
  expect_equal(result$value(), "alpha")
})

test_that("RSDynamicHash: setObject overwrites equal object", {
  h <- RSDynamicHash$new()
  key1 <- RSString$stringWith("dup")
  key2 <- RSString$stringWith("dup")  # different instance, same value
  h$setObject(key1)
  h$setObject(key2)  # should overwrite
  all <- h$allObjectsAsList()
  expect_equal(length(all), 1)
  expect_equal(all[[1]]$value(), "dup")
})

test_that("RSDynamicHash: containsObject reflects registration", {
  h <- RSDynamicHash$new()
  key <- RSString$stringWith("present")
  expect_false(h$containsObject(key))
  h$setObject(key)
  expect_true(h$containsObject(key))
})

test_that("RSDynamicHash: removeObject deletes object", {
  h <- RSDynamicHash$new()
  key <- RSString$stringWith("bye")
  h$setObject(key)
  expect_true(h$containsObject(key))
  h$removeObject(key)
  expect_false(h$containsObject(key))
})

test_that("RSDynamicHash: getObject errors if object not registered", {
  h <- RSDynamicHash$new()
  key <- RSString$stringWith("ghost")
  expect_null(h$getObject(key))
})

test_that("RSDynamicHash: allObjectsAsList returns correct list", {
  h <- RSDynamicHash$new()
  keys <- lapply(c("a", "b", "c"), RSString$stringWith)
  lapply(keys, h$setObject)
  all_vals <- h$allObjectsAsList()
  all_raw <- vapply(all_vals, function(obj) obj$value(), character(1))
  expect_setequal(all_raw, c("a", "b", "c"))
})

test_that("RSDynamicHash: count reflects number of stored objects", {
  h <- RSDynamicHash$new()
  expect_equal(h$count(), 0)
  h$setObject(RSString$stringWith("one"))
  h$setObject(RSString$stringWith("two"))
  expect_equal(h$count(), 2)
})
