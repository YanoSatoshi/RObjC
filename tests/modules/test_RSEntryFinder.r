library(testthat)

test_that("RSEntryFinder: findIndexByLinear works with RSString", {
  entries <- lapply(c("apple", "banana", "candy"), RSString$stringWith)
  target <- RSString$stringWith("banana")
  not_found <- RSString$stringWith("orange")
  finder <- RSEntryFinder$module()
  idx <- finder$findIndexByLinear(entries, target)
  expect_equal(idx, 2L)
  idx_nf <- finder$findIndexByLinear(entries, not_found)
  expect_true(is.na(idx_nf))
})

test_that("RSEntryFinder: findByLinear returns correct RSString", {
  entries <- lapply(c("a", "b", "c"), RSString$stringWith)
  target <- RSString$stringWith("c")
  res <- RSEntryFinder$module()$findByLinear(entries, target)
  expect_s3_class(res, "RSString")
  expect_equal(res$value(), "c")
})

test_that("RSEntryFinder: findIndexByBinary works on sorted RSString", {
  entries <- lapply(c("apple", "banana", "candy", "date", "fig"), RSString$stringWith)
  target <- RSString$stringWith("candy")
  not_found <- RSString$stringWith("grape")
  finder <- RSEntryFinder$module()
  idx <- finder$findIndexByBinary(entries, target)
  expect_equal(idx, 3L)
  idx_nf <- finder$findIndexByBinary(entries, not_found)
  expect_true(is.na(idx_nf))
})

test_that("RSEntryFinder: findByBinary returns correct object or NULL", {
  entries <- lapply(c("a", "b", "c", "d"), RSString$stringWith)
  target <- RSString$stringWith("b")
  not_found <- RSString$stringWith("z")
  finder <- RSEntryFinder$module()
  found <- finder$findByBinary(entries, target)
  not_found_result <- finder$findByBinary(entries, not_found)
  expect_s3_class(found, "RSString")
  expect_equal(found$value(), "b")
  expect_null(not_found_result)
})
