test_that("package registrations restore instances and generator references", {
  source <- RSNumber$new(42L)
  archive <- RSKeyedArchiver$new()$archive(list(source, RSNumber, RSCoding))
  records <- archive$records
  expect_true(all(vapply(records, function(record)
    identical(record$package, "robjc") && nzchar(record$class_id), logical(1))))

  restored <- RSKeyedUnarchiver$new(unserialize(serialize(archive, NULL)))$unarchive()
  expect_identical(restored[[1L]]$nativeValue(), 42L)
  expect_identical(restored[[2L]], RSNumber)
  expect_identical(restored[[3L]], RSCoding)
})

test_that("registration validates namespace ownership and is idempotent", {
  expect_identical(
    robjc_register_class("robjc", "RSNumber", "RSNumber", RSNumber),
    RSNumber
  )
  expect_error(
    robjc_register_class("robjc", "RSNumber", "RSObject", RSObject),
    "already registered"
  )
  expect_error(
    robjc_register_class("robjc", "wrong", "RSNumber", RSObject),
    "named binding"
  )
  expect_error(
    robjc_register_protocol("robjc", "wrong", "RSNumber", RSNumber),
    "protocol generator"
  )
})

test_that("unregistered classes retain the explicit resolver path", {
  Local <- RSObjectClass(
    "LocalCodingNode",
    public = list(
      encodeWithCoder = function(coder) coder$encodeObjectForKey(7L, "value"),
      initWithCoder = function(coder) {
        self$value <- coder$decodeObjectForKey("value")
        invisible(self)
      },
      value = NULL
    )
  )
  archive <- RSKeyedArchiver$new()$archive(Local$new())
  expect_null(archive$records[[1L]]$package)
  expect_error(RSKeyedUnarchiver$new(archive)$unarchive(), "Unresolved archive classes")
  restored <- RSKeyedUnarchiver$new(archive, function(...) Local)$unarchive()
  expect_identical(restored$value, 7L)
})

test_that("an extension package registers its class in .onLoad", {
  package_name <- "robjcextfixture"
  root <- tempfile("robjc-extension-")
  package_dir <- file.path(root, package_name)
  library_dir <- file.path(root, "library")
  dir.create(file.path(package_dir, "R"), recursive = TRUE)
  dir.create(library_dir)
  old_libpaths <- .libPaths()
  on.exit({
    if (isNamespaceLoaded(package_name)) unloadNamespace(package_name)
    .libPaths(old_libpaths)
    unlink(root, recursive = TRUE)
  }, add = TRUE)

  writeLines(c(
    "Package: robjcextfixture",
    "Title: Minimal RObjC Extension Fixture",
    "Version: 0.0.1",
    "Description: Tests registration and keyed archive restoration.",
    "License: MIT",
    "Encoding: UTF-8",
    "Imports: robjc"
  ), file.path(package_dir, "DESCRIPTION"))
  writeLines("export(ExtensionWidget)", file.path(package_dir, "NAMESPACE"))
  writeLines(c(
    'ExtensionWidget <- robjc::RSObjectClass("ExtensionWidget", version = "1.0",',
    "  public = list(",
    "    value = NULL,",
    "    initialize = function(value) {",
    "      super$initialize()",
    "      self$value <- value",
    "      invisible(self)",
    "    },",
    "    encodeWithCoder = function(coder) {",
    '      coder$encodeObjectForKey(self$value, "value")',
    "    },",
    "    initWithCoder = function(coder) {",
    '      self$value <- coder$decodeObjectForKey("value")',
    "      invisible(self)",
    "    }",
    "  )",
    ")"
  ), file.path(package_dir, "R", "Widget.R"))
  writeLines(c(
    ".onLoad <- function(libname, pkgname) {",
    "  robjc::robjc_register_class(pkgname,",
    '    id = "widget", binding = "ExtensionWidget",',
    "    generator = ExtensionWidget)",
    "}"
  ), file.path(package_dir, "R", "zzz.R"))

  install_output <- suppressWarnings(system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", paste0("--library=", shQuote(library_dir)),
      shQuote(package_dir)),
    stdout = TRUE, stderr = TRUE
  ))
  install_status <- attr(install_output, "status")
  if (is.null(install_status)) install_status <- 0L
  expect_identical(install_status, 0L,
                   info = paste(install_output, collapse = "\n"))
  .libPaths(c(library_dir, old_libpaths))

  namespace <- loadNamespace(package_name, lib.loc = library_dir)
  extension_class <- get("ExtensionWidget", envir = namespace)
  archive <- RSKeyedArchiver$new()$archive(extension_class$new("saved"))
  record <- archive$records[[1L]]
  expect_identical(record$package, package_name)
  expect_identical(record$class_id, "widget")

  rm(extension_class, namespace)
  unloadNamespace(package_name)
  decoder <- RSKeyedUnarchiver$new(unserialize(serialize(archive, NULL)))
  classes <- decoder$showAllClasses(remove_default = FALSE)
  expect_identical(classes$decoder_name, "ExtensionWidget")
  expect_identical(classes$status, "automatic")
  expect_identical(decoder$decode()$value, "saved")
})
