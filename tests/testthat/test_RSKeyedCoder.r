test_that("keyed coding preserves shared and self references", {
  initialize_calls <- 0L
  Node <- RSObjectClass(
    "CoderRoundTripNode",
    version = "1.0.0",
    public = list(
      value = NULL,
      next_node = NULL,
      initialize = function(value) {
        initialize_calls <<- initialize_calls + 1L
        super$initialize()
        self$value <- value
        invisible(self)
      },
      encodeWithCoder = function(coder) {
        coder$encodeObjectForKey(self$value, "value")
        coder$encodeObjectForKey(self$next_node, "next")
      },
      initWithCoder = function(coder) {
        self$value <- coder$decodeObjectForKey("value")
        self$next_node <- coder$decodeObjectForKey("next")
        invisible(self)
      }
    )
  )

  source <- Node$new("hello")
  expect_identical(initialize_calls, 1L)
  source$next_node <- source
  archive <- RSKeyedArchiver$new()$archive(list(source, source))
  expect_identical(archive$format, "robjc-keyed-1")
  expect_length(archive$records, 1L)
  expect_identical(archive$root$values[[1L]]$id, robjc_get_pointer(source))
  expect_identical(names(archive$records), robjc_get_pointer(source))

  restored <- RSKeyedUnarchiver$new(
    unserialize(serialize(archive, NULL)),
    function(class_name, version, kind) {
      expect_identical(class_name, "CoderRoundTripNode")
      expect_identical(version, "1.0.0")
      expect_identical(kind, "instance")
      Node
    }
  )$unarchive()
  expect_identical(restored[[1L]], restored[[2L]])
  expect_identical(restored[[1L]], restored[[1L]]$next_node)
  expect_identical(restored[[1L]]$value, "hello")
  expect_identical(initialize_calls, 1L)
})

test_that("keyed coding validates fields and missing references", {
  coder <- RSKeyedArchiver$new()
  expect_error(coder$encodeObjectForKey(1L, "x"), "No object")
  expect_error(coder$archive(RSObject$new()), "encodeWithCoder")

  broken <- list(
    format = "robjc-keyed-1",
    root = list(type = "ref", id = "1"),
    records = list()
  )
  expect_error(
    RSKeyedUnarchiver$new(broken, function(...) RSObject)$unarchive(),
    "Missing archive record"
  )
})
