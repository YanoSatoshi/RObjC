# TODO: REMOVE ON RELEASE
tmp <- "register_rs_module"
if (Sys.getenv("DEBUG_MODE") == "TRUE" && exists(tmp, environment(), inherits = FALSE)) {
  if (bindingIsLocked(tmp, environment())) {
    unlockBinding(tmp, environment())
    cat(sprintf("Unlocks binding %s.\n", tmp))
  } else {
    cat(sprintf("%s binding is locked.\n", tmp))
  }
}


#' @include RSModule.r
#' @title Register a Singleton Instance of an RSModule Subclass
#'
#' @description
#' Registers a singleton module object for a given `RSModule` subclass.
#' The instance is stored in the class's internal `robjc_isa$slot` field and
#' can be accessed via the automatically defined `$module()` method.
#'
#' If the accessor is already defined, an error is raised to prevent double registration.
#' Optionally, the instance's fields can be locked using `lockVariables()` for immutability.
#' The `slot` field itself is locked using `lockBinding()` to prevent reassignment.
#'
#' This function is intended for internal use as part of the module initialization
#' process in the legacy registration workflow. New module classes use
#' `RSSingletonClass()`, which creates and stores `$instance` during declaration.
#'
#' @param klass The R6 class object (must inherit from `RSModule`)
#' @param obj The singleton instance of the module to register
#' @param lock_all Logical. If `TRUE`, the instance's fields are locked using `lockVariables()`.
#'        Defaults to `FALSE`.
#'
#' @return The registered singleton object (invisibly).
#'
#' @seealso
#' \code{\link{RSModule}} – The base class for singleton modules in the robjc system.
#'
#' @keywords internal
register_rs_module <- function(klass, obj, lock_all = FALSE) {
  stopifnot(obj$isKindOf(RSModule))
  klass$robjc_isa$slot <- obj
  if (is.null(klass$module)) {
    klass$module <- function() klass$robjc_isa$slot
  } else {
    stop(sprintf("%s has already registered by register_rs_module."))
  }
  if (lock_all) obj$lockVariables()
  lockBinding("slot", klass$robjc_isa)
  invisible(obj)
}
lockBinding("register_rs_module", environment())
