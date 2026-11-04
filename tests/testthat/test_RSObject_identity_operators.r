test_that("RSObject equality operators compare references, not values", {
  first <- RSNumber$new(1L)
  alias <- first
  equal_value <- RSNumber$new(1L)

  expect_true(first == alias)
  expect_false(first != alias)
  expect_false(first == equal_value)
  expect_true(first != equal_value)
  expect_true(first$isEqual(equal_value))
  expect_false(first == NULL)
  expect_false(NULL == first)
  expect_true(first != NULL)
  expect_true(NULL != first)
  expect_false(first == RSNumber)
})
