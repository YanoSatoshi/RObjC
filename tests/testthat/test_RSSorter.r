test_that("RSSorter: sortByProxyKey correctly sorts RSNumber", {
  entries <- list(
    RSNumber_withValue(10),
    RSNumber_withValue(5),
    RSNumber_withValue(7)
  )
  sorted <- RSSorter$instance$sortByProxyKey(entries, function(x) x$value())
  values <- vapply(sorted, function(x) x$value(), numeric(1))
  expect_equal(values, c(5, 7, 10))
})

test_that("RSSorter: sortByCompare uses RSScalar$compare()", {
  entries <- list(
    RSString_withValue("banana"),
    RSString_withValue("apple"),
    RSString_withValue("candy")
  )
  compare_fn <- function(a, b) a$compare(b)
  sorted <- RSSorter$instance$sortByCompare(entries, compare_fn)
  values <- vapply(sorted, function(x) x$value(), character(1))
  expect_equal(values, c("apple", "banana", "candy"))
})
