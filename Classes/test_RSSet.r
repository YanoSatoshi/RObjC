library(testthat)

test_that("RSSet: count and containsObject work", {
  s <- RSSet$new(list(
    RSString$stringWith("a"),
    RSString$stringWith("b")
  ))
  expect_equal(s$count(), 2)
  expect_true(s$containsObject(RSString$stringWith("a")))
  expect_false(s$containsObject(RSString$stringWith("z")))
})

test_that("RSSet: allObjectsAsList returns correct content", {
  s <- RSSet$new(list(
    RSString$stringWith("a"),
    RSString$stringWith("b")
  ))
  values <- vapply(s$allObjectsAsList(), function(x) x$value(), character(1))
  expect_setequal(values, c("a", "b"))
})

test_that("RSSet: addObject is prohibited", {
  s <- RSSet$new(list())
  expect_error(
    s$addObject(RSString$stringWith("x")),
    msg_immutable_try_mutate(s$class())
  )
})

test_that("RSSet: removeObject is prohibited", {
  s <- RSSet$new(list(RSString$stringWith("y")))
  expect_error(
    s$removeObject(RSString$stringWith("y")),
    msg_immutable_try_mutate(s$class())
  )
})

test_that("RSSet: removeAll is prohibited", {
  s <- RSSet$new(list(RSString$stringWith("z")))
  expect_error(
    s$removeAllObjects(),
    msg_immutable_try_mutate(s$class())
  )
})

test_that("RSSet: copyWithEnv returns RSSet", {
  s <- RSSet$new(list(RSString$stringWith("k")))
  copied <- s$copyWithEnv(new.env())
  expect_s3_class(copied, "RSSet")
  expect_equal(copied$count(), 1)
})

test_that("RSSet: mutableCopyWithEnv returns RSMutableSet", {
  s <- RSSet$new(list(RSString$stringWith("m")))
  mcopy <- s$mutableCopyWithEnv(new.env())
  expect_s3_class(mcopy, "RSMutableSet")
  expect_equal(mcopy$count(), 1)
})
