library(testthat)

test_that("RSCopying: requiredMethods returns copyWithEnv", {
  expect_equal(RSCopying$protocol()$requiredMethods(), "copyWithEnv")
})

test_that("RSCopying: copyObjectWithEnv works on RSObject with method", {
  df <- tibble::tibble(a = 1:2)
  obj <- RSDataFrame$new(df)
  copied <- RSCopying$protocol()$copyObjectWithEnv(obj, new.env())
  expect_s3_class(copied, "RSDataFrame")
  expect_false(identical(obj, copied))
  expect_equal(copied$getData(), df)
})

test_that("RSCopying: error if RSObject but copyWithEnv not defined", {
  BadClass <- R6::R6Class(
    "BadClass",
    inherit = RSObject,
    public = list(initialize = function() {super$initialize()})
  )
  dummy <- BadClass$new()
  expect_error(
    RSCopying$protocol()$copyObjectWithEnv(dummy, new.env()),
    "does not implement copyWithEnv()"
  )
})

test_that("RSCopying: error if not RSObject-based at all", {
  alien <- R6::R6Class("AlienClass", public = list(x = 1))$new()
  expect_error(
    RSCopying$protocol()$copyObjectWithEnv(alien, new.env()),
    "Protocol cannot be implimented other than RSObject"
  )
})
