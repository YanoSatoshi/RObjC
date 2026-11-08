test_that("collections directly declare and implement RSListing", {
  collections <- list(
    RSArray$new(), RSMutableArray$new(),
    RSSet$new(), RSMutableSet$new(),
    RSDictionary$new(), RSMutableDictionary$new(),
    RSVector$new(integer()), RSMutableVector$new(integer())
  )
  for (collection in collections) {
    klass <- collection$class()
    expect_true(any(vapply(klass$robjc_isa$protocols, identical, logical(1),
                           RSListing)), info = klass$classname)
    expect_true(collection$conformsTo(RSListing), info = klass$classname)
    expect_identical(protocol_getMissingContracts(klass), character(0),
                     info = klass$classname)
    expect_true(is.list(collection$asList()), info = klass$classname)
  }
  expect_false(RSObject$new()$conformsTo(RSListing))
})

test_that("RSListing exposes the expected iteration elements", {
  key <- RSString_withValue("key")
  value <- RSNumber_withValue(1L)
  expect_identical(RSArray$new(list(key))$asList(), list(key))
  expect_identical(RSSet$new(list(key))$asList(), list(key))
  dictionary <- RSDictionary$new(keys = list(key), values = list(value))
  expect_identical(dictionary$asList(), list(key))
  expect_identical(RSVector$new(c(2L, 3L))$asList(), list(2L, 3L))
  dates <- as.Date(c("2024-01-01", "2024-01-02"))
  expect_identical(RSMutableVector$new(dates)$asList(), as.list(dates))
})
