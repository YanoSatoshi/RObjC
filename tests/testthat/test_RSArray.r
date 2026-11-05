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

test_that("RSArray: immutable API has no mutation methods", {
  ary <- RSArray$new(list(RSString_withValue("x")))
  for (method in c("addObject", "insertObjectAtIndex", "replaceObjectAtIndex",
                   "removeObjectAtIndex", "removeAllObjects")) {
    expect_false(ary$respondsTo(method))
  }
  expect_false(ary$isKindOf(RSMutableArray))
  mutable <- ary$mutableCopy()
  expect_true(mutable$isKindOf(RSArray))
  expect_true(mutable$respondsTo("addObject"))
})

test_that("RSArray: mutable copies share elements but not list storage", {
  element <- RSString_withValue("same")
  original <- RSArray$new(list(element))
  mutable <- original$mutableCopy()
  expect_identical(original$allObjects(), original)
  expect_identical(mutable$allObjects()$objectAtIndex(1L), element)
  expect_false(identical(mutable$allObjects(), mutable))
  mutable$replaceObjectAtIndex(RSString_withValue("changed"), 1L)
  expect_identical(original$objectAtIndex(1L), element)
  expect_identical(mutable$objectAtIndex(1L)$value(), "changed")
})
