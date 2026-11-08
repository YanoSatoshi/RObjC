test_that("mutable and immutable vectors restore atomic values", {
  vectors <- list(
    RSMutableVector$new(c(1L, NA_integer_, 3L)),
    RSVector$new(factor(c("b", NA), levels = c("b", "a"), ordered = TRUE)),
    RSVector$new(as.Date(c("2026-10-29", "2026-10-30")))
  )
  archive <- RSKeyedArchiver$new()$encode(vectors)
  restored <- RSKeyedUnarchiver$new(
    unserialize(serialize(archive, NULL)),
    function(name, version, kind) switch(name,
      RSMutableVector = RSMutableVector,
      RSVector = RSVector,
      stop("Unexpected class: ", name)
    )
  )$decode()
  for (i in seq_along(vectors)) {
    expect_identical(restored[[i]]$class(), vectors[[i]]$class())
    expect_identical(restored[[i]]$asVector(), vectors[[i]]$asVector())
    expect_identical(restored[[i]]$hash(), vectors[[i]]$hash())
  }
  expect_identical(restored[[1L]]$appendValues(4L), restored[[1L]])
  expect_false(restored[[2L]]$respondsTo("appendValues"))
})

test_that("mutable and immutable data frames restore their data", {
  data <- tibble::tibble(
    id = c(1L, 2L),
    category = factor(c("b", "a"), levels = c("b", "a")),
    date = as.Date(c("2026-10-29", "2026-10-30"))
  )
  sources <- list(
    RSMutableDataFrame$new(data),
    RSDataFrame$new(data)
  )
  archive <- RSKeyedArchiver$new()$encode(sources)
  restored <- RSKeyedUnarchiver$new(
    unserialize(serialize(archive, NULL)),
    function(name, version, kind) switch(name,
      RSMutableDataFrame = RSMutableDataFrame,
      RSDataFrame = RSDataFrame,
      stop("Unexpected class: ", name)
    )
  )$decode()
  for (i in seq_along(sources)) {
    expect_identical(restored[[i]]$class(), sources[[i]]$class())
    expect_identical(restored[[i]]$getDataFrame(), sources[[i]]$getDataFrame())
    expect_identical(restored[[i]]$hash(), sources[[i]]$hash())
  }
  restored[[1L]]$setColumn("id", c(3L, 4L))
  expect_identical(restored[[1L]]$getColumn("id"), c(3L, 4L))
  expect_false(restored[[2L]]$respondsTo("setDataFrame"))
  expect_false(restored[[2L]]$respondsTo("setColumn"))
})
