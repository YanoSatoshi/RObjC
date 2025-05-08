test_that("RSSorter: sortByProxyKey correctly sorts RSNumber", {
  entries <- list(
    RSNumber$numberWith(10),
    RSNumber$numberWith(5),
    RSNumber$numberWith(7)
  )
  sorted <- RSSorter$module()$sortByProxyKey(entries, function(x) x$value())
  values <- vapply(sorted, function(x) x$value(), numeric(1))
  expect_equal(values, c(5, 7, 10))
})

test_that("RSSorter: sortByCompare uses RSScalar$compare()", {
  entries <- list(
    RSString$stringWith("banana"),
    RSString$stringWith("apple"),
    RSString$stringWith("candy")
  )
  compare_fn <- function(a, b) a$compare(b)
  sorted <- RSSorter$module()$sortByCompare(entries, compare_fn)
  values <- vapply(sorted, function(x) x$value(), character(1))
  expect_equal(values, c("apple", "banana", "candy"))
})
