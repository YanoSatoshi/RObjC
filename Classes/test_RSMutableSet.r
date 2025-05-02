library(testthat)

test_that("RSMutableSet: addObject stores unique elements", {
  s <- RSMutableSet$new()
  s$addObject(RSString$stringWith("a"))
  s$addObject(RSString$stringWith("b"))
  s$addObject(RSString$stringWith("a"))  # 重複追加
  expect_equal(s$count(), 2)
})

test_that("RSMutableSet: containsObject returns TRUE if present", {
  s <- RSMutableSet$new()
  key <- RSString$stringWith("x")
  s$addObject(key)
  expect_true(s$containsObject(key))
  expect_false(s$containsObject(RSString$stringWith("y")))
})

test_that("RSMutableSet: removeObject eliminates element", {
  s <- RSMutableSet$new()
  key <- RSString$stringWith("z")
  s$addObject(key)
  s$removeObject(key)
  expect_false(s$containsObject(key))
})

test_that("RSMutableSet: allObjectsAsList returns all elements", {
  s <- RSMutableSet$new()
  keys <- lapply(c("a", "b", "c"), RSString$stringWith)
  lapply(keys, s$addObject)
  raw <- vapply(s$allObjectsAsList(), function(o) o$value(), character(1))
  expect_setequal(raw, c("a", "b", "c"))
})

test_that("RSMutableSet: removeAllObjects clears all elements", {
  s <- RSMutableSet$new()
  s$addObject(RSString$stringWith("tmp"))
  s$removeAllObjects()
  expect_equal(s$count(), 0)
})

test_that("RSMutableSet: copyWithEnv returns immutable RSSet", {
  s <- RSMutableSet$new()
  s$addObject(RSString$stringWith("x"))
  copied <- s$copyWithEnv(new.env())
  expect_s3_class(copied, "RSSet")
  expect_equal(copied$count(), 1)
})

test_that("RSMutableSet: mutableCopyWithEnv returns RSMutableSet", {
  s <- RSMutableSet$new()
  s$addObject(RSString$stringWith("x"))
  mcopy <- s$mutableCopyWithEnv(new.env())
  expect_s3_class(mcopy, "RSMutableSet")
  expect_equal(mcopy$count(), 1)
})

test_that("RSMutableSet: set operations work (union, intersect, diff)", {
  a <- RSMutableSet$new()
  b <- RSMutableSet$new()
  a$addObject(RSString$stringWith("a"))
  a$addObject(RSString$stringWith("b"))
  b$addObject(RSString$stringWith("b"))
  b$addObject(RSString$stringWith("c"))
  u <- a$unionSet(b)
  i <- a$intersectSet(b)
  d <- a$differenceSet(b)
  expect_setequal(
    vapply(u$allObjectsAsList(), function(x) x$value(), character(1)),
    c("a", "b", "c")
  )
  expect_setequal(
    vapply(i$allObjectsAsList(), function(x) x$value(), character(1)),
    c("b")
  )
  expect_setequal(
    vapply(d$allObjectsAsList(), function(x) x$value(), character(1)),
    c("a")
  )
})
