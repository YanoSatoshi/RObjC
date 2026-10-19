test_that("robjc_read_rds resolves standard and custom classes after serialization", {
  Note <- RSObjectClass("ReadRdsGuideNote", version = "1.0")
  Notebook <- RSObjectClass(
    "ReadRdsGuideNotebook", version = "1.0",
    public = list(
      notes = NULL,
      initialize = function(notes) {
        super$initialize()
        self$notes <- notes
        invisible(self)
      },
      objectsForEncoding = function() c(super$objectsForEncoding(), self$notes)
    )
  )
  path <- tempfile(fileext = ".rds")
  on.exit({
    unlink(path)
    RSClassEncoder$instance$refresh()
  }, add = TRUE)

  note <- Note$new()
  robjc_save_rds(Notebook$new(list(note, note)), path)
  wrapped <- lapply(list(Notebook, Note), RSWrappedClass$new)
  class_map <- RSDictionary_withObjectsForKeys(
    objects = wrapped,
    keys = lapply(wrapped, function(x) x$classID())
  )
  loaded <- robjc_read_rds(path, class_map = class_map)

  expect_identical(loaded$class(), Notebook)
  expect_identical(loaded$notes[[1L]]$class(), Note)
  expect_identical(loaded$notes[[1L]], loaded$notes[[2L]])

  robjc_save_rds(RSString_withValue("hello"), path)
  standard <- robjc_read_rds(path)
  expect_identical(standard$class(), RSString)
  expect_identical(standard$value(), "hello")
})
