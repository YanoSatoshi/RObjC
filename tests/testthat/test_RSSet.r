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
