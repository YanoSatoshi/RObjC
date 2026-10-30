#' @include api_setup.r

# Keep only names and namespace addresses here. A generator reference would keep
# an unloaded package's old class alive and could be used after a reload.
.robjc_class_registry <- new.env(hash = TRUE, parent = emptyenv())

.robjc_registry_string <- function(value, label) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value))
    stop(sprintf("`%s` must be one nonempty string.", label), call. = FALSE)
  value
}

.robjc_registry_bucket <- function(package, kind, create = FALSE) {
  if (!exists(package, envir = .robjc_class_registry, inherits = FALSE)) {
    if (!create) return(NULL)
    .robjc_class_registry[[package]] <- new.env(hash = TRUE, parent = emptyenv())
  }
  packages <- .robjc_class_registry[[package]]
  if (!exists(kind, envir = packages, inherits = FALSE)) {
    if (!create) return(NULL)
    packages[[kind]] <- new.env(hash = TRUE, parent = emptyenv())
  }
  packages[[kind]]
}

.robjc_register_generator <- function(package, id, binding, generator, kind) {
  package <- .robjc_registry_string(package, "package")
  id <- .robjc_registry_string(id, "id")
  binding <- .robjc_registry_string(binding, "binding")
  if (!isNamespaceLoaded(package))
    stop(sprintf("Package '%s' must be loaded before registration.", package), call. = FALSE)
  ns <- asNamespace(package)
  if (!exists(binding, envir = ns, inherits = FALSE) ||
      !identical(get(binding, envir = ns, inherits = FALSE), generator))
    stop("`generator` must be the named binding in the package namespace.", call. = FALSE)
  valid <- if (identical(kind, "class")) robjc_isClass(generator) else robjc_isProtocol(generator)
  if (!valid)
    stop(sprintf("`generator` must be an RObjC %s generator.", kind), call. = FALSE)

  namespace_id <- robjc_get_pointer(ns)
  bucket <- .robjc_registry_bucket(package, kind, create = TRUE)
  if (exists(id, envir = bucket, inherits = FALSE)) {
    old <- bucket[[id]]
    if (identical(old$namespace_id, namespace_id) &&
        !identical(old$binding, binding))
      stop(sprintf("Identifier '%s' is already registered in package '%s'.", id, package), call. = FALSE)
  }
  for (other_id in ls(bucket, all.names = TRUE)) {
    old <- bucket[[other_id]]
    if (!identical(other_id, id) &&
        identical(old$namespace_id, namespace_id) &&
        identical(get(old$binding, envir = ns, inherits = FALSE), generator))
      stop("A generator cannot be registered under multiple identifiers.", call. = FALSE)
  }
  bucket[[id]] <- list(binding = binding, namespace_id = namespace_id)
  invisible(generator)
}

#' Register a package class for keyed archives
#'
#' Call this in the defining package's `.onLoad()` after its generator has been
#' created. `id` is a stable archive identifier; keep it unchanged when the R
#' binding or displayed class name changes. The registration stores names only,
#' and decoding retrieves the current generator from the package namespace.
#'
#' @section Extension package setup:
#' For a package named `mypkg` with a generator bound as `MyClass`, add this
#' call to its `.onLoad()` after `MyClass` is defined:
#' ```r
#' robjc::robjc_register_class(pkgname, "my-class-v1", "MyClass", MyClass)
#' ```
#' The same pattern works for protocols with [robjc_register_protocol()].
#' The package should import the registration function or use `robjc::`.
#'
#' @param package Name of the package defining the generator.
#' @param id Stable identifier within the package.
#' @param binding Name of the generator binding in that package's namespace.
#' @param generator The generator held by `binding`.
#' @return `generator`, invisibly.
#' @export
robjc_register_class <- function(package, id, binding, generator) {
  .robjc_register_generator(package, id, binding, generator, "class")
}

#' Register a package protocol for keyed archives
#'
#' Protocol generator references can appear in archives. Call this from the
#' defining package's `.onLoad()` with a stable `id`.
#'
#' @inheritParams robjc_register_class
#' @return `generator`, invisibly.
#' @export
robjc_register_protocol <- function(package, id, binding, generator) {
  .robjc_register_generator(package, id, binding, generator, "protocol")
}

.robjc_registered_identity <- function(generator) {
  kind <- if (robjc_isClass(generator)) "class" else "protocol"
  for (package in ls(.robjc_class_registry, all.names = TRUE)) {
    if (!isNamespaceLoaded(package)) next
    ns <- asNamespace(package)
    bucket <- .robjc_registry_bucket(package, kind)
    if (is.null(bucket)) next
    for (id in ls(bucket, all.names = TRUE)) {
      entry <- bucket[[id]]
      if (!identical(entry$namespace_id, robjc_get_pointer(ns)) ||
          !exists(entry$binding, envir = ns, inherits = FALSE)) next
      if (identical(get(entry$binding, envir = ns, inherits = FALSE), generator))
        return(list(package = package, id = id))
    }
  }
  NULL
}

.robjc_resolve_registered <- function(package, id, kind) {
  package <- .robjc_registry_string(package, "package")
  id <- .robjc_registry_string(id, "id")
  if (!isNamespaceLoaded(package) && !requireNamespace(package, quietly = TRUE))
    stop(sprintf("Package '%s' is unavailable for decoding.", package), call. = FALSE)
  ns <- asNamespace(package)
  bucket <- .robjc_registry_bucket(package, if (identical(kind, "protocol")) "protocol" else "class")
  if (is.null(bucket) || !exists(id, envir = bucket, inherits = FALSE))
    stop(sprintf("No registered generator for %s::%s.", package, id), call. = FALSE)
  entry <- bucket[[id]]
  if (!identical(entry$namespace_id, robjc_get_pointer(ns)))
    stop(sprintf("Stale registration for %s::%s; re-register in .onLoad().", package, id), call. = FALSE)
  if (!exists(entry$binding, envir = ns, inherits = FALSE))
    stop(sprintf("Registered binding '%s' is missing.", entry$binding), call. = FALSE)
  get(entry$binding, envir = ns, inherits = FALSE)
}
