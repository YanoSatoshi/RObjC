test_that("RSMutableCopying: requiredMethods returns mutableCopy", {
  expect_equal(RSMutableCopying$protocol()$requiredMethods(), "mutableCopy")
})

test_that("RSMutableCopying: error if mutableCopyWithEnvir not implemented", {
  BadMutable <- R6::R6Class(
    "BadMutable",
    inherit = RSObject,
    public = list(
      initialize = function() { super$initialize() },
      dummy = function() NULL
    )
  )
  obj <- BadMutable$new()
  expect_error(
    RSMutableCopying$protocol()$mutableCopyObjectWithEnvir(obj, new.env()),
    "does not implement mutableCopyWithEnvir"
  )
})

test_that("RSMutableCopying: error if not RSObject-based at all", {
  alien <- R6::R6Class("Alien", public = list(x = 123))$new()
  expect_error(
    RSMutableCopying$protocol()$mutableCopyObjectWithEnvir(alien, new.env()),
    "Protocol cannot be implimented other than RSObject"
  )
})
