test_that("robjc_get_pointer identifies RObjC instances and generators", {
  address <- robjc_get_pointer
  object <- RSString$new("a")
  alias <- object

  expect_identical(address(object), address(alias))
  expect_identical(address(object), object$objectID())
  expect_false(identical(address(object), address(RSString$new("a"))))
  expect_match(address(RSString), "^0x[0-9a-f]+$")
})

test_that("robjc_get_pointer accepts R objects without a type restriction", {
  address <- robjc_get_pointer
  values <- list(a = 1L)
  frame <- data.frame(a = 1L)
  tibble <- tibble::tibble(a = 1L)

  for (value in list(values, frame, tibble))
    expect_match(address(value), "^0x[0-9a-f]+$")

  for (value in list(1:3, c("a", "b"), identity, NULL))
    expect_match(address(value), "^0x[0-9a-f]+$")

  alias <- values
  expect_identical(address(values), address(alias))
  attr(values, "tag") <- "changed"
  expect_false(identical(address(values), address(alias)))
})
