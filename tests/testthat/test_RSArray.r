test_that("RSArray: copy returns identical copy", {
  ary <- RSArray$new(list(
    RSString_withValue("a"),
    RSString_withValue("b")
  ))
  copied <- ary$copy()
  expect_true(identical(ary, copied))
})

test_that("RSArray: mutableCopy returns RSMutableArray", {
  ary <- RSArray$new(list(RSString_withValue("x")))
  mcopy <- ary$mutableCopy()
  expect_equal(mcopy$objectAtIndex(1)$value(), "x")
})

test_that("RSArray: addObject raises error", {
  ary <- RSArray$new(list())
  expect_error(
    ary$addObject(RSString_withValue("fail")),
    msg_immutable_try_mutate(ary$class())
  )
})

test_that("RSArray: insertObjectAtIndex raises error", {
  ary <- RSArray$new(list(RSString_withValue("X")))
  expect_error(
    ary$insertObjectAtIndex(RSString_withValue("Y"), 1),
    msg_immutable_try_mutate(ary$class())
  )
  expect_error(
    ary$insertObjectAtIndex(obj = RSString_withValue("Y"), index = 1L),
    msg_immutable_try_mutate(ary$class()), fixed = TRUE
  )
})

test_that("RSArray: replaceObjectAtIndex raises error", {
  ary <- RSArray$new(list(RSString_withValue("Z")))
  expect_error(
    ary$replaceObjectAtIndex(RSString_withValue("W"), 1),
    msg_immutable_try_mutate(ary$class())
  )
  expect_error(
    ary$replaceObjectAtIndex(obj = RSString_withValue("W"), index = 1L),
    msg_immutable_try_mutate(ary$class()), fixed = TRUE
  )
})

test_that("RSArray: removeObjectAtIndex and removeAllObjects raise error", {
  ary <- RSArray$new(list(RSString_withValue("delete")))
  expect_error(ary$removeObjectAtIndex(1), msg_immutable_try_mutate(ary$class()))
  expect_error(ary$removeObjectAtIndex(index = 1L),
               msg_immutable_try_mutate(ary$class()), fixed = TRUE)
  expect_error(ary$removeAllObjects(), msg_immutable_try_mutate(ary$class()))
})
