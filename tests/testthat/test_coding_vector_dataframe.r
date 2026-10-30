test_that("mutable and immutable vectors restore atomic values", {
  vectors <- list(
    RSMutableVector$new(c(1L, NA_integer_, 3L)),
    RSVector$new(factor(c("b", NA), levels = c("b", "a"), ordered = TRUE)),
    RSVector$new(as.Date(c("2026-10-29", "2026-10-30")))
  )
  archive <- RSKeyedArchiver$new()$archive(vectors)
  restored <- RSKeyedUnarchiver$new(
    unserialize(serialize(archive, NULL)),
    function(name, version, kind) switch(name,
      RSMutableVector = RSMutableVector,
      RSVector = RSVector,
      stop("Unexpected class: ", name)
    )
  )$unarchive()
  for (i in seq_along(vectors)) {
    expect_identical(restored[[i]]$class(), vectors[[i]]$class())
    expect_identical(restored[[i]]$asVector(), vectors[[i]]$asVector())
    expect_identical(restored[[i]]$hashOperator(), RSHashOperator$instance)
  }
  expect_identical(restored[[1L]]$appendValues(4L), restored[[1L]])
  expect_error(restored[[2L]]$appendValues("c"), "immutable")
})

test_that("vector keeps a custom hash operator reference", {
  Operator <- RSObjectClass("VectorCodingOperator", version = "1.0.0",
    public = list(
      hashForScalar = function(val) 1L,
      encodeWithCoder = function(coder) invisible(self),
      initWithCoder = function(coder) invisible(self)
    ))
  operator <- Operator$new()
  source <- RSMutableVector$new(c(2L, 4L), hash_operator = operator)
  archive <- RSKeyedArchiver$new()$archive(list(source, operator))
  restored <- RSKeyedUnarchiver$new(archive,
    function(name, version, kind) switch(name,
      RSMutableVector = RSMutableVector,
      VectorCodingOperator = Operator,
      stop("Unexpected class: ", name)
    ))$unarchive()
  expect_identical(restored[[1L]]$hashOperator(), restored[[2L]])
  expect_identical(restored[[1L]]$hashesOfValues(), c(1L, 1L))
})

test_that("mutable and immutable data frames restore data and keys", {
  data <- tibble::tibble(
    id = c(1L, 2L),
    category = factor(c("b", "a"), levels = c("b", "a")),
    date = as.Date(c("2026-10-29", "2026-10-30"))
  )
  sources <- list(
    RSMutableDataFrame$new(data, key_columns = "id"),
    RSDataFrame$new(data, key_columns = c("id", "date"))
  )
  archive <- RSKeyedArchiver$new()$archive(sources)
  restored <- RSKeyedUnarchiver$new(
    unserialize(serialize(archive, NULL)),
    function(name, version, kind) switch(name,
      RSMutableDataFrame = RSMutableDataFrame,
      RSDataFrame = RSDataFrame,
      stop("Unexpected class: ", name)
    )
  )$unarchive()
  for (i in seq_along(sources)) {
    expect_identical(restored[[i]]$class(), sources[[i]]$class())
    expect_identical(restored[[i]]$getData(), sources[[i]]$getData())
    expect_identical(restored[[i]]$keyColumns(), sources[[i]]$keyColumns())
  }
  restored[[1L]]$setColumn("id", c(3L, 4L))
  expect_identical(restored[[1L]]$getColumn("id"), c(3L, 4L))
  expect_error(restored[[2L]]$setData(data), "immutable")
})
