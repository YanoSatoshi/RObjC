library(testthat)

test_that("RSDictionary: objectForKey returns correct value", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString$stringWith("v1"), RSString$stringWith("k1"),
    RSString$stringWith("v2"), RSString$stringWith("k2")
  ))
  v <- d$objectForKey(RSString$stringWith("k1"))
  expect_s3_class(v, "RSString")
  expect_equal(v$value(), "v1")
})

test_that("RSDictionary: objectForKey returns NULL for missing key", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString$stringWith("value"), RSString$stringWith("key")
  ))
  expect_null(d$objectForKey(RSString$stringWith("absent")))
})

test_that("RSDictionary: count returns correct size", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString$stringWith("v1"), RSString$stringWith("k1"),
    RSString$stringWith("v2"), RSString$stringWith("k2")
  ))
  expect_equal(d$count(), 2)
})

test_that("RSDictionary: mutation methods are prohibited", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString$stringWith("v"), RSString$stringWith("k")
  ))
  expect_error(
    d$setObjectForKey(RSString$stringWith("v2"), RSString$stringWith("k")),
    msg_immutable_try_mutate(d$class())
  )
  expect_error(
    d$removeObjectForKey(RSString$stringWith("k")),
    msg_immutable_try_mutate(d$class())
  )
  expect_error(
    d$removeAllObjects(),
    msg_immutable_try_mutate(d$class())
  )
})

test_that("RSDictionary: copyWithEnv returns RSDictionary", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString$stringWith("v"), RSString$stringWith("k")
  ))
  copy <- d$copyWithEnv()
  expect_s3_class(copy, "RSDictionary")
  expect_equal(copy$objectForKey(RSString$stringWith("k"))$value(), "v")
})

test_that("RSDictionary: mutableCopyWithEnv returns RSMutableDictionary", {
  d <- RSDictionary$new(objects_and_keys = list(
    RSString$stringWith("v"), RSString$stringWith("k")
  ))
  mcopy <- d$mutableCopyWithEnv()
  expect_s3_class(mcopy, "RSMutableDictionary")
  expect_equal(mcopy$objectForKey(RSString$stringWith("k"))$value(), "v")
})
