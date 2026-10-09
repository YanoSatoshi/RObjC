test_that("RSConcreteSet stores members without copying them", {
  storage <- RSConcreteSet$new()
  original <- RSString_withValue("member")
  equal_object <- RSString_withValue("member")

  expect_identical(storage$add(original), storage)
  expect_identical(storage$add(equal_object), storage)
  expect_equal(storage$count(), 1)
  expect_true(storage$contains(equal_object))
  expect_identical(storage$member(equal_object), original)
  expect_null(storage$member(RSString_withValue("missing")))
})

test_that("RSConcreteSet removes and clears members", {
  storage <- RSConcreteSet$new()
  storage$add(RSString_withValue("a"))
  storage$add(RSString_withValue("b"))

  expect_true(storage$remove(RSString_withValue("a")))
  expect_false(storage$remove(RSString_withValue("missing")))
  expect_equal(storage$count(), 1)

  expect_identical(storage$clear(), storage)
  expect_equal(storage$count(), 0)
  expect_length(storage$allObjects(), 0)
})

test_that("RSConcreteSet duplicate has independent native storage", {
  storage <- RSConcreteSet$new()
  storage$add(RSString_withValue("a"))

  copied <- storage$duplicate()
  storage$add(RSString_withValue("b"))
  copied$remove(RSString_withValue("a"))

  expect_true(storage$contains(RSString_withValue("a")))
  expect_true(storage$contains(RSString_withValue("b")))
  expect_equal(copied$count(), 0)
})

test_that("RSConcreteSet performs in-place set operations", {
  left <- RSConcreteSet$new()
  left$add(RSString_withValue("a"))
  left$add(RSString_withValue("b"))

  right <- RSConcreteSet$new()
  right$add(RSString_withValue("b"))
  right$add(RSString_withValue("c"))

  union <- left$duplicate()
  expect_identical(union$uniteWith(right), union)
  expect_setequal(
    vapply(union$allObjects(), function(item) item$value(), character(1)),
    c("a", "b", "c")
  )

  intersection <- left$duplicate()
  expect_identical(intersection$intersectWith(right), intersection)
  expect_equal(intersection$count(), 1)
  expect_equal(intersection$member(RSString_withValue("b"))$value(), "b")

  difference <- left$duplicate()
  expect_identical(difference$subtractWith(right), difference)
  expect_equal(difference$count(), 1)
  expect_equal(difference$member(RSString_withValue("a"))$value(), "a")
})

test_that("RSConcreteSet handles operations with itself", {
  storage <- RSConcreteSet$new()
  storage$add(RSString_withValue("a"))

  storage$uniteWith(storage)
  expect_equal(storage$count(), 1)

  storage$intersectWith(storage)
  expect_equal(storage$count(), 1)

  storage$subtractWith(storage)
  expect_equal(storage$count(), 0)
})
