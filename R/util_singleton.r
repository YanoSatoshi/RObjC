# TODO: REMOVE ON RELEASE
tmp <- "register_rs_singleton"
if (Sys.getenv("DEBUG_MODE") == "TRUE" && exists(tmp, environment(), inherits = FALSE)) {
  if (bindingIsLocked(tmp, environment())) {
    unlockBinding(tmp, environment())
    cat(sprintf("Unlocks binding %s.\n", tmp))
  } else {
    cat(sprintf("%s binding is locked.\n", tmp))
  }
}


#' @title Register a Singleton Instance for an RSSingleton Subclass
#'
#' @description
#' Registers a singleton instance of an `RSSingleton` subclass and stores it in the
#' class's internal `robjc_isa$slot` field. An accessor method `$instance()` is automatically
#' defined on the class to retrieve the registered singleton.
#'
#' After registration, the singleton slot is locked using `lockBinding()` to prevent
#' reassignment. By default, the instance's fields are also locked using `lockVariables()`
#' to ensure immutability. This can be disabled via the `lock_all` argument.
#'
#' This helper is intended for internal use within the robjc system and supports
#' both singleton-style objects and placeholder values such as `RSNull`.
#' New singleton classes use `RSSingletonClass()` and access `$instance`
#' directly; they do not require this registration helper.
#'
#' @param klass The R6 class object to which the singleton belongs.
#' @param obj The singleton instance to register.
#' @param lock_all Logical. If `TRUE` (default), the instance's fields will be locked
#'   using `lockVariables()`. Set to `FALSE` in controlled contexts such as testing
#'   or dynamic reconfiguration.
#'
#' @return The registered singleton instance (invisibly).
#'
#' @seealso
#' \code{\link{RSSingleton}} – The base class for robjc-style singletons.
#'
#' @keywords internal
register_rs_singleton <- function(klass, obj, lock_all = TRUE) {
  stopifnot(obj$isKindOf(RSSingleton) || obj$isMemberOf(RSNull))
  klass$robjc_isa$slot <- obj
  if (is.null(klass$instance)) {
    klass$instance <- function() klass$robjc_isa$slot
  } else {
    stop(sprintf("%s has already registered by register_rs_singleton."))
  }
  if (lock_all) obj$lockVariables()
  lockBinding("slot", klass$robjc_isa)
  invisible(obj)
}
lockBinding("register_rs_singleton", environment())
