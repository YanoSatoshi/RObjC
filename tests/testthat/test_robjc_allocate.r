test_that("robjc_allocate creates an RObjC instance without initialize", {
  initialize_calls <- 0L
  Example <- RSObjectClass(
    "AllocationTestExample",
    private = list(.value = NULL),
    public = list(
      initialize = function(value) {
        initialize_calls <<- initialize_calls + 1L
        super$initialize()
        private$.value <- value
        invisible(self)
      },
      setValue = function(value) {
        private$.value <- value
        invisible(self)
      },
      value = function() private$.value
    )
  )

  object <- robjc_allocate(Example)
  expect_identical(initialize_calls, 0L)
  expect_true(object$isMemberOf(Example))
  expect_null(object$value())
  object$setValue(42L)
  expect_identical(object$value(), 42L)
  expect_error(robjc_allocate(list()), "RObjC object class generator")
})
