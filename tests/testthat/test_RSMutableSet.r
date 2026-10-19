test_that("RSMutableSet: addObject stores unique elements", {
  s <- RSMutableSet$new()
  s$addObject(RSString_withValue("a"))
  s$addObject(RSString_withValue("b"))
  s$addObject(RSString_withValue("a"))  # 重複追加
  expect_equal(s$count(), 2)
})

test_that("RSMutableSet validates a member hash before storage", {
  calls <- 0L
  ProbeItem <- RSObjectClass(
    "MutableSetHashProbe", super_class = RSObject,
    public = list(hash = function() {
      calls <<- calls + 1L
      stop("hash failure")
    })
  )
  item <- ProbeItem$new()
  set <- RSMutableSet$new()

  expect_error(set$addObject(item), "hash failure", fixed = TRUE)
  expect_identical(calls, 1L)
  expect_equal(set$count(), 0)
  expect_error(RSSet$new(objects = list(item)), "hash failure", fixed = TRUE)
})

test_that("RSMutableSet: containsObject returns TRUE if present", {
  s <- RSMutableSet$new()
  key <- RSString_withValue("x")
  s$addObject(key)
  expect_true(s$containsObject(key))
  expect_false(s$containsObject(RSString_withValue("y")))
})

test_that("RSMutableSet: retains the inserted representative", {
  original <- RSString_withValue("member")
  equal_object <- RSString_withValue("member")
  s <- RSMutableSet$new(list(original, equal_object))

  expect_equal(s$count(), 1)
  expect_identical(s$member(equal_object), original)
})

test_that("RSMutableSet: represents NULL with RSNull", {
  s <- RSMutableSet$new(list(NULL))

  expect_true(s$containsObject(NULL))
  expect_true(s$member(NULL)$isKindOf(RSNull))
  expect_true(s$anyObject()$isKindOf(RSNull))
})

test_that("RSMutableSet accepts class and protocol generators", {
  protocol <- RSProtocolClass("MutableSetGeneratorProtocol")
  object <- RSString_withValue("member")
  set <- RSMutableSet$new(list(RSString, protocol, object))
  same <- RSSet$new(list(protocol, object, RSString))

  expect_equal(set$count(), 3L)
  expect_true(set$containsObject(RSString))
  expect_identical(set$member(protocol), protocol)
  expect_true(set$isEqual(same))
  expect_identical(set$hash(), same$hash())
  expect_identical(set$objectsForEncoding(), list(object))
  expect_true(set$intersectSet(RSSet$new(list(protocol)))$containsObject(protocol))
  expect_true(set$uniteSet(RSSet$new(list(protocol)))$containsObject(protocol))
  expect_false(set$subtractSet(RSSet$new(list(protocol)))$containsObject(protocol))
  set$removeObject(RSString)
  expect_false(set$containsObject(RSString))
  set$addObject(RSString)
  expect_true(set$containsObject(RSString))
})

test_that("RSMutableSet: removeObject eliminates element", {
  s <- RSMutableSet$new()
  key <- RSString_withValue("z")
  s$addObject(key)
  s$removeObject(key)
  expect_false(s$containsObject(key))
})

test_that("RSMutableSet: allObjectsAsList returns all elements", {
  s <- RSMutableSet$new()
  keys <- lapply(c("a", "b", "c"), RSString_withValue)
  lapply(keys, s$addObject)
  raw <- vapply(s$allObjectsAsList(), function(o) o$value(), character(1))
  expect_setequal(raw, c("a", "b", "c"))
})

test_that("RSMutableSet: removeAllObjects clears all elements", {
  s <- RSMutableSet$new()
  s$addObject(RSString_withValue("tmp"))
  s$removeAllObjects()
  expect_equal(s$count(), 0)
})

test_that("RSMutableSet: copy returns immutable RSSet", {
  s <- RSMutableSet$new()
  s$addObject(RSString_withValue("x"))
  copied <- s$copy()
  expect_equal(copied$count(), 1)
})

test_that("RSMutableSet: mutableCopy returns RSMutableSet", {
  s <- RSMutableSet$new()
  s$addObject(RSString_withValue("x"))
  mcopy <- s$mutableCopy()
  expect_s3_class(mcopy, "RSMutableSet")
  expect_equal(mcopy$count(), 1)

  mcopy$addObject(RSString_withValue("y"))
  expect_equal(s$count(), 1)
  expect_equal(mcopy$count(), 2)
})

test_that("RSMutableSet: set operations work (union, intersect, diff)", {
  a <- RSMutableSet$new()
  b <- RSMutableSet$new()
  a$addObject(RSString_withValue("a"))
  a$addObject(RSString_withValue("b"))
  b$addObject(RSString_withValue("b"))
  b$addObject(RSString_withValue("c"))
  u <- a$uniteSet(b)
  i <- a$intersectSet(b)
  d <- a$subtractSet(b)
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
  expect_equal(a$count(), 2)
  expect_equal(b$count(), 2)
})
