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

test_that("RSSet: addObject is prohibited", {
  s <- RSSet$new(list())
  expect_error(
    s$addObject(RSString_withValue("x")),
    msg_immutable_try_mutate(s$class())
  )
})

test_that("RSSet: removeObject is prohibited", {
  s <- RSSet$new(list(RSString_withValue("y")))
  expect_error(
    s$removeObject(RSString_withValue("y")),
    msg_immutable_try_mutate(s$class())
  )
})

test_that("RSSet: removeAll is prohibited", {
  s <- RSSet$new(list(RSString_withValue("z")))
  expect_error(
    s$removeAllObjects(),
    msg_immutable_try_mutate(s$class())
  )
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

test_that("RSSet: setWithSet creates independent native storage", {
  source <- RSMutableSet$new(list(RSString_withValue("a")))
  copied <- RSSet$setWithSet(source)

  source$addObject(RSString_withValue("b"))

  expect_s3_class(copied, "RSSet")
  expect_equal(copied$count(), 1)
  expect_true(copied$containsObject(RSString_withValue("a")))
})
