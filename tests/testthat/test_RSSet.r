test_that("RSSet: count and containsObject work", {
  s <- RSSet$new(list(
    RSString_withValue("a"),
    RSString_withValue("b")
  ))
  expect_equal(s$count(), 2)
  expect_true(s$containsObject(RSString_withValue("a")))
  expect_false(s$containsObject(RSString_withValue("z")))
})

test_that("RSSet: allObjectsAsList returns correct content", {
  s <- RSSet$new(list(
    RSString_withValue("a"),
    RSString_withValue("b")
  ))
  values <- vapply(s$allObjectsAsList(), function(x) x$value(), character(1))
  expect_setequal(values, c("a", "b"))
})

test_that("RSSet: immutable API does not expose mutation methods", {
  s <- RSSet$new(list())
  expect_false(s$respondsTo("addObject"))
  expect_false(s$respondsTo("removeObject"))
  expect_false(s$respondsTo("removeAllObjects"))
  expect_true(s$isKindOf(RSSet))
  expect_false(s$isKindOf(RSMutableSet))
  mutable <- RSMutableSet$new()
  expect_true(mutable$isKindOf(RSSet))
  expect_true(mutable$respondsTo("addObject"))
})

test_that("RSSet: copy returns RSSet", {
  s <- RSSet$new(list(RSString_withValue("k")))
  copied <- s$copy()
  expect_identical(s, copied)
})

test_that("RSSet: mutableCopy returns RSMutableSet", {
  s <- RSSet$new(list(RSString_withValue("m")))
  mcopy <- s$mutableCopy()
  expect_s3_class(mcopy, "RSMutableSet")
  expect_equal(mcopy$count(), 1)

  mcopy$addObject(RSString_withValue("n"))
  expect_equal(s$count(), 1)
  expect_equal(mcopy$count(), 2)
})

test_that("RSSet_withSet creates independent native storage", {
  source <- RSMutableSet$new(list(RSString_withValue("a")))
  copied <- RSSet_withSet(source)

  source$addObject(RSString_withValue("b"))

  expect_s3_class(copied, "RSSet")
  expect_equal(copied$count(), 1)
  expect_true(copied$containsObject(RSString_withValue("a")))
})
test_that("RSSet rejects R NULL members", {
  expect_error(RSSet$new(list(NULL)), "RSNull$null", fixed = TRUE)
  set <- RSSet$new(list(RSNull$null))
  expect_error(set$member(NULL), "robjc_wrap_null", fixed = TRUE)
  expect_error(set$containsObject(NULL), "robjc_wrap_null", fixed = TRUE)
  expect_identical(set$member(RSNull$null), RSNull$null)
})

test_that("RSSet subset and additions preserve immutable operands", {
  a <- RSString_withValue("a")
  b <- RSString_withValue("b")
  base <- RSSet$new(list(a))
  larger <- RSMutableSet$new(list(a, b))
  expect_true(base$isSubsetOfSet(larger))
  expect_true(base$isSubsetOfSet(base))
  expect_false(larger$isSubsetOfSet(base))
  expect_true(RSSet$new()$isSubsetOfSet(base))
  expect_true(RSSet$new()$isSubsetOfSet(RSSet$new()))
  expect_error(base$isSubsetOfSet(RSArray$new()), "set")

  added <- base$setByAddingObject(b)
  expect_true(added$containsObject(b))
  expect_equal(base$count(), 1)
  expect_equal(base$setByAddingObject(RSString_withValue("a"))$count(), 1)
  expect_error(base$setByAddingObject(NULL), "RSNull")

  combined <- base$setByAddingObjectsFromSet(larger)
  expect_equal(combined$count(), 2)
  expect_equal(base$count(), 1)
  expect_equal(larger$count(), 2)

  from_array <- base$setByAddingObjectsFromArray(RSArray$new(list(b)))
  expect_equal(from_array$count(), 2)
  expect_error(base$setByAddingObjectsFromArray(larger), "RSArray")
})
