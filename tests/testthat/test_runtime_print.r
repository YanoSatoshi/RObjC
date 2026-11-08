test_that("RObjC generator printing uses generator names and hides ISA", {
  Widget <- RSObjectClass(
    "VisibleWidget",
    private = list(secret = 1L),
    version = "0.2.0"
  )

  output <- format(Widget)
  expect_length(output, 1L)
  expect_match(output, "<VisibleWidget> RObjC object generator", fixed = TRUE)
  expect_match(output, "Inherits from: <RSObject>", fixed = TRUE)
  expect_match(output, "secret: 1", fixed = TRUE)
  expect_false(grepl(".__isa", output, fixed = TRUE))
  expect_identical(paste(utils::capture.output(print(Widget)), collapse = "\n"), output)

  expect_match(format(RSModule), "<RSModule> RObjC object generator", fixed = TRUE)
  expect_match(format(RSCopying), "<RSCopying> RObjC protocol generator", fixed = TRUE)
})

test_that("RObjC identity printing uses the owning generator name", {
  output <- format(RSObject$robjc_isa)
  expect_match(output, "<RSObject> RObjC object identity", fixed = TRUE)
  for (field in ls(RSObject$robjc_isa, all.names = TRUE)) {
    expect_match(output, paste0(field, ":"), fixed = TRUE)
  }
  expect_false(grepl(".__isa", output, fixed = TRUE))
  expect_identical(paste(utils::capture.output(print(RSObject$robjc_isa)), collapse = "\n"), output)

  expect_match(
    format(RSCopying$robjc_isa),
    "<RSCopying> RObjC protocol identity",
    fixed = TRUE
  )
  expect_match(format(RSCopying$robjc_isa), "slot: copy", fixed = TRUE)
  expect_match(format(RSModule$robjc_isa), "slot: <RSModule instance>", fixed = TRUE)
})

test_that("RObjC instance printing includes private data and controls methods", {
  obj <- RSString$new("hello")
  class(obj) <- c("AlternateName", "RSObject")

  output <- format(obj)
  expect_length(output, 1L)
  expect_match(output, "<RSString> RObjC instance", fixed = TRUE)
  expect_match(output, "Inherits from: <RSScalar>", fixed = TRUE)
  expect_match(output, ".val: hello", fixed = TRUE)
  expect_false(grepl("class: function", output, fixed = TRUE))
  expect_false(grepl(".comparable: function", output, fixed = TRUE))
  expect_false(grepl(".__isa", output, fixed = TRUE))
  expect_identical(paste(utils::capture.output(print(obj)), collapse = "\n"), output)

  method_output <- format(obj, method = TRUE)
  expect_match(method_output, "class: function", fixed = TRUE)
  expect_match(method_output, ".val: hello", fixed = TRUE)
  expect_match(method_output, ".comparable: function", fixed = TRUE)
  expect_identical(
    paste(utils::capture.output(print(obj, method = TRUE)), collapse = "\n"),
    method_output
  )
})

test_that("RSObject descriptions use the S3 formatter options", {
  VisibleItem <- RSObjectClass(
    "VisibleItem",
    private = list(secret = 7L),
    public = list(label = "shown", reveal = function() private$secret)
  )
  item <- VisibleItem$new()

  ordinary <- item$description()
  expect_identical(ordinary, format(item))
  expect_identical(ordinary, paste(utils::capture.output(print(item)), collapse = "\n"))
  expect_match(ordinary, "label: shown", fixed = TRUE)
  expect_match(ordinary, "secret: 7", fixed = TRUE)
  expect_false(grepl("reveal: function", ordinary, fixed = TRUE))
  expect_false(grepl(".__isa", ordinary, fixed = TRUE))

  detailed <- item$debugDescription()
  expect_identical(detailed, format(item, method = TRUE))
  expect_identical(detailed, paste(utils::capture.output(print(item, method = TRUE)), collapse = "\n"))
  expect_match(detailed, "secret: 7", fixed = TRUE)
  expect_match(detailed, "reveal: function", fixed = TRUE)
  expect_false(grepl(".__isa", detailed, fixed = TRUE))
})
