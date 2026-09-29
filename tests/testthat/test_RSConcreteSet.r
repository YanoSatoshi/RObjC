test_that("RSConcreteSet stores members without copying them", {
  storage <- RSConcreteSet$new()
  original <- RSString$stringWith("member")
  equal_object <- RSString$stringWith("member")

  expect_identical(storage$add(original), storage)
  expect_identical(storage$add(equal_object), storage)
  expect_equal(storage$count(), 1)
  expect_true(storage$contains(equal_object))
  expect_identical(storage$member(equal_object), original)
  expect_null(storage$member(RSString$stringWith("missing")))
})

test_that("RSConcreteSet removes and clears members", {
  storage <- RSConcreteSet$new()
  storage$add(RSString$stringWith("a"))
  storage$add(RSString$stringWith("b"))

  expect_true(storage$remove(RSString$stringWith("a")))
  expect_false(storage$remove(RSString$stringWith("missing")))
  expect_equal(storage$count(), 1)

  expect_identical(storage$clear(), storage)
  expect_equal(storage$count(), 0)
  expect_length(storage$allObjects(), 0)
})

test_that("RSConcreteSet duplicate has independent native storage", {
  storage <- RSConcreteSet$new()
  storage$add(RSString$stringWith("a"))

  copied <- storage$duplicate()
  storage$add(RSString$stringWith("b"))
  copied$remove(RSString$stringWith("a"))

  expect_true(storage$contains(RSString$stringWith("a")))
  expect_true(storage$contains(RSString$stringWith("b")))
  expect_equal(copied$count(), 0)
})

test_that("RSConcreteSet performs in-place set operations", {
  left <- RSConcreteSet$new()
  left$add(RSString$stringWith("a"))
  left$add(RSString$stringWith("b"))

  right <- RSConcreteSet$new()
  right$add(RSString$stringWith("b"))
  right$add(RSString$stringWith("c"))

  union <- left$duplicate()
  expect_identical(union$uniteWith(right), union)
  expect_setequal(
    vapply(union$allObjects(), function(item) item$value(), character(1)),
    c("a", "b", "c")
  )

  intersection <- left$duplicate()
  expect_identical(intersection$intersectWith(right), intersection)
  expect_equal(intersection$count(), 1)
  expect_equal(intersection$member(RSString$stringWith("b"))$value(), "b")

  difference <- left$duplicate()
  expect_identical(difference$subtractWith(right), difference)
  expect_equal(difference$count(), 1)
  expect_equal(difference$member(RSString$stringWith("a"))$value(), "a")
})

test_that("RSConcreteSet handles operations with itself", {
  storage <- RSConcreteSet$new()
  storage$add(RSString$stringWith("a"))

  storage$uniteWith(storage)
  expect_equal(storage$count(), 1)

  storage$intersectWith(storage)
  expect_equal(storage$count(), 1)

  storage$subtractWith(storage)
  expect_equal(storage$count(), 0)
})
