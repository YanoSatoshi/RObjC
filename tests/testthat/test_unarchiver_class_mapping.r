test_that("unarchiver inspects classes before allocating instances", {
  source <- RSNumber$new(4L)
  archive <- RSKeyedArchiver$new()$archive(list(source, source, RSString))
  allocations <- 0L
  factory <- function(generator) {
    allocations <<- allocations + 1L
    robjc_allocate(generator)
  }
  expect_output(
    decoder <- RSKeyedUnarchiver$new(archive, make_instance = factory),
    "showAllClasses"
  )
  expect_identical(allocations, 0L)
  expect_s3_class(decoder$showAllClasses(), "tbl_df")
  expect_equal(nrow(decoder$showAllClasses()), 0L)
  expect_error(decoder$showAllClasses(remove_default = NA), "remove_default")
  classes <- decoder$showAllClasses(remove_default = FALSE)
  expect_s3_class(classes, "tbl_df")
  expect_setequal(classes$class_name, c("RSNumber", "RSString"))
  expect_true(all(classes$status == "automatic"))
  expect_true(all(vapply(classes$decoder_class, robjc_isClass, logical(1))))
  dictionary <- decoder$dictionaryWithAllClasses()
  expect_true(dictionary$isKindOf(RSMutableDictionary))
  expect_equal(dictionary$count(), nrow(classes))
  number_key <- classes$class_key[classes$class_name == "RSNumber"]
  expect_identical(dictionary$objectForKey(RSString$new(number_key)), RSNumber)
  expect_identical(decoder$showClass(classes$class_key[[1L]])$class_key,
                   classes$class_key[[1L]])
  expect_identical(allocations, 0L)
  restored <- decoder$decode()
  expect_identical(allocations, 1L)
  expect_identical(restored[[1L]], restored[[2L]])
  expect_identical(restored[[3L]], RSString)
  explicit <- RSKeyedUnarchiver$new(archive, function(class_name, version, kind) {
    switch(class_name, RSNumber = RSNumber, RSString = RSString)
  })
  expect_equal(nrow(explicit$showAllClasses()), 2L)
  expect_true(all(explicit$showAllClasses()$status == "resolver"))
  classes$decoder_class[classes$class_name == "RSNumber"] <- list(RSObject)
  decoder$setClasses(classes)
  expect_identical(decoder$showAllClasses()$class_name, "RSNumber")
  expect_error(decoder$decode(), "does not implement initWithCoder")
  expect_identical(allocations, 1L)
})

test_that("edited tibble and dictionary mappings drive decoding", {
  Old <- RSObjectClass("OldMappedClass", public = list(
    initialize = function(value) {
      super$initialize()
      self$value <- value
      invisible(self)
    },
    value = NULL,
    encodeWithCoder = function(coder) coder$encodeObjectForKey(self$value, "value"),
    initWithCoder = function(coder) {
      self$value <- coder$decodeObjectForKey("value")
      invisible(self)
    }
  ))
  New <- RSObjectClass("NewMappedClass", public = list(
    value = NULL,
    initWithCoder = function(coder) {
      self$value <- coder$decodeObjectForKey("value")
      invisible(self)
    }
  ))
  archive <- RSKeyedArchiver$new()$archive(Old$new(19L))
  expect_error(
    RSKeyedUnarchiver$new(archive, function(...) stop("resolver failure"))$showAllClasses(),
    "resolver failure"
  )
  decoder <- suppressMessages(RSKeyedUnarchiver$new(archive))
  classes <- decoder$showAllClasses()
  expect_identical(classes$status, "unresolved")
  unresolved <- decoder$dictionaryWithAllClasses()
  expect_identical(unresolved$objectForKey(RSString$new(classes$class_key)), RSNull$null)
  decoder$setClasses(unresolved)
  expect_identical(decoder$showAllClasses()$status, "unresolved")
  expect_error(decoder$decode(), "Unresolved archive classes")
  expect_error(decoder$setClasses(tibble::tibble(
    class_key = classes$class_key, decoder_class = list(RSCoding)
  )), "Invalid decoder generator")
  expect_identical(decoder$showAllClasses()$status, "unresolved")
  classes$decoder_class[1L] <- list(New)
  decoder$setClasses(classes)
  expect_identical(decoder$showClass(classes$class_key)$decoder_name, "NewMappedClass")
  expect_identical(decoder$showAllClasses()$status, "manual")
  expect_true(decoder$decode()$isMemberOf(New))

  other <- RSKeyedUnarchiver$new(archive)
  mapping <- other$dictionaryWithAllClasses()
  mapping$setObjectForKey(New, RSString$new(classes$class_key))
  expect_identical(other$showClass(classes$class_key)$status, "unresolved")
  other$setClasses(mapping)
  expect_identical(other$showClass(classes$class_key)$status, "manual")
  expect_identical(other$decode()$value, 19L)
  expect_error(other$showClass("missing"), "Unknown archive class key")
  expect_error(other$setClasses(tibble::tibble(
    class_key = "missing", decoder_class = list(New)
  )), "Invalid or duplicate")
})
