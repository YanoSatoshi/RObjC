library(testthat)

test_that("RSArray: copyWithEnv returns equal but not identical copy", {
  ary <- RSArray$new(list(
    RSString$stringWith("a"),
    RSString$stringWith("b")
  ))
  copied <- ary$copyWithEnv(new.env())
  expect_s3_class(copied, "RSArray")
  expect_equal(copied$allObjectsAsList()[[1]]$value(), "a")
  expect_false(identical(ary, copied))
})

test_that("RSArray: mutableCopyWithEnv returns RSMutableArray", {
  ary <- RSArray$new(list(RSString$stringWith("x")))
  mcopy <- ary$mutableCopyWithEnv(new.env())
  expect_s3_class(mcopy, "RSMutableArray")
  expect_equal(mcopy$objectAtIndex(1)$value(), "x")
})

test_that("RSArray: addObject raises error", {
  ary <- RSArray$new(list())
  expect_error(
    ary$addObject(RSString$stringWith("fail")),
    msg_immutable_try_mutate(ary$class())
  )
})

test_that("RSArray: insertObjectAtIndex raises error", {
  ary <- RSArray$new(list(RSString$stringWith("X")))
  expect_error(
    ary$insertObjectAtIndex(RSString$stringWith("Y"), 1),
    msg_immutable_try_mutate(ary$class())
  )
})

test_that("RSArray: replaceObjectAtIndex raises error", {
  ary <- RSArray$new(list(RSString$stringWith("Z")))
  expect_error(
    ary$replaceObjectAtIndex(RSString$stringWith("W"), 1),
    msg_immutable_try_mutate(ary$class())
  )
})

test_that("RSArray: removeObjectAtIndex and removeAllObjects raise error", {
  ary <- RSArray$new(list(RSString$stringWith("delete")))
  expect_error(ary$removeObjectAtIndex(1), msg_immutable_try_mutate(ary$class()))
  expect_error(ary$removeAllObjects(), msg_immutable_try_mutate(ary$class()))
})
