test_that("RObjC generator printing uses generator names and hides ISA", {
  Widget <- RSObjectClass(
    "VisibleWidget",
    private = list(secret = 1L),
    version = "0.1.0 beta"
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

test_that("RObjC instance printing uses the generator name and hides private ISA", {
  obj <- RSString$new("hello")
  class(obj) <- c("AlternateName", "RSObject")

  output <- format(obj)
  expect_length(output, 1L)
  expect_match(output, "<RSString> RObjC instance", fixed = TRUE)
  expect_match(output, "Inherits from: <RSScalar>", fixed = TRUE)
  expect_match(output, ".val: hello", fixed = TRUE)
  expect_false(grepl(".__isa", output, fixed = TRUE))
  expect_identical(paste(utils::capture.output(print(obj)), collapse = "\n"), output)
})
