test_that("RSMutableArray: addObject and count work", {
  ary <- RSMutableArray$new()
  expect_equal(ary$count(), 0)
  ary$addObject(RSString_withValue("apple"))
  ary$addObject(RSString_withValue("banana"))
  expect_equal(ary$count(), 2)
})

test_that("RSMutableArray: objectAtIndex returns correct element", {
  ary <- RSMutableArray$new()
  ary$addObject(RSString_withValue("a"))
  ary$addObject(RSString_withValue("b"))
  expect_equal(ary$objectAtIndex(2)$value(), "b")
  expect_equal(ary$objectAtIndex(1)$value(), "a")
})

test_that("RSMutableArray: insertObjectAtIndex works", {
  ary <- RSMutableArray$new()
  ary$addObject(RSString_withValue("a"))
  ary$addObject(RSString_withValue("c"))
  ary$insertObjectAtIndex(RSString_withValue("b"), 2)
  values <- vapply(ary$allObjectsAsList(), function(x) x$value(), character(1))
  expect_equal(values, c("a", "b", "c"))
})

test_that("RSMutableArray: replaceObjectAtIndex works", {
  ary <- RSMutableArray$new()
  ary$addObject(RSString_withValue("X"))
  ary$replaceObjectAtIndex(RSString_withValue("Y"), 1)
  expect_equal(ary$objectAtIndex(1)$value(), "Y")
})

test_that("RSMutableArray: removeObjectAtIndex removes correct item", {
  ary <- RSMutableArray$new()
  ary$addObject(RSString_withValue("a"))
  ary$addObject(RSString_withValue("b"))
  ary$addObject(RSString_withValue("c"))
  ary$removeObjectAtIndex(2)
  values <- vapply(ary$allObjectsAsList(), function(x) x$value(), character(1))
  expect_equal(values, c("a", "c"))
})

test_that("RSMutableArray: removeAllObjects resets array", {
  ary <- RSMutableArray$new()
  ary$addObject(RSString_withValue("a"))
  ary$addObject(RSString_withValue("b"))
  ary$removeAllObjects()
  expect_equal(ary$count(), 0)
})
