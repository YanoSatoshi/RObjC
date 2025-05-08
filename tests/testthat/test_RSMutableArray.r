test_that("RSMutableArray: addObject and count work", {
  ary <- RSMutableArray$new()
  expect_equal(ary$count(), 0)
  ary$addObject(RSString$stringWith("apple"))
  ary$addObject(RSString$stringWith("banana"))
  expect_equal(ary$count(), 2)
})

test_that("RSMutableArray: objectAtIndex returns correct element", {
  ary <- RSMutableArray$new()
  ary$addObject(RSString$stringWith("a"))
  ary$addObject(RSString$stringWith("b"))
  expect_equal(ary$objectAtIndex(2)$value(), "b")
  expect_equal(ary$objectAtIndex(1)$value(), "a")
})

test_that("RSMutableArray: insertObjectAtIndex works", {
  ary <- RSMutableArray$new()
  ary$addObject(RSString$stringWith("a"))
  ary$addObject(RSString$stringWith("c"))
  ary$insertObjectAtIndex(RSString$stringWith("b"), 2)
  values <- vapply(ary$allObjectsAsList(), function(x) x$value(), character(1))
  expect_equal(values, c("a", "b", "c"))
})

test_that("RSMutableArray: replaceObjectAtIndex works", {
  ary <- RSMutableArray$new()
  ary$addObject(RSString$stringWith("X"))
  ary$replaceObjectAtIndex(RSString$stringWith("Y"), 1)
  expect_equal(ary$objectAtIndex(1)$value(), "Y")
})

test_that("RSMutableArray: removeObjectAtIndex removes correct item", {
  ary <- RSMutableArray$new()
  ary$addObject(RSString$stringWith("a"))
  ary$addObject(RSString$stringWith("b"))
  ary$addObject(RSString$stringWith("c"))
  ary$removeObjectAtIndex(2)
  values <- vapply(ary$allObjectsAsList(), function(x) x$value(), character(1))
  expect_equal(values, c("a", "c"))
})

test_that("RSMutableArray: removeAllObjects resets array", {
  ary <- RSMutableArray$new()
  ary$addObject(RSString$stringWith("a"))
  ary$addObject(RSString$stringWith("b"))
  ary$removeAllObjects()
  expect_equal(ary$count(), 0)
})
