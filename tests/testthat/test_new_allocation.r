test_that("RObjC new returns the initialize result", {
  allocated <- NULL
  Example <- RSObjectClass(
    "NewAllocationResultExample",
    public = list(
      initialize = function(value) {
        allocated <<- self
        value
      }
    )
  )

  expect_identical(Example$new(42L), 42L)
  expect_true(allocated$isMemberOf(Example))
  expect_true(bindingIsLocked("initialize", allocated))
})

test_that("singleton new does not reinitialize its shared instance", {
  calls <- 0L
  Example <- RSSingletonClass(
    "NewAllocationSingletonExample",
    public = list(
      initialize = function() {
        calls <<- calls + 1L
        super$initialize()
        invisible(self)
      }
    )
  )

  expect_identical(calls, 1L)
  expect_identical(Example$new(), Example$instance)
  expect_identical(calls, 1L)
})
