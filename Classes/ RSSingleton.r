#' RSSingleton
#'
#' Base class for implementing singleton-style system manager classes
#' within the RS framework, independent of RSModule.
#'
#' This class ensures that only one instance of a subclass is created,
#' and enforces access to that instance via an explicit constructor function \code{$instance()}.
#' Attempting to instantiate the class directly using \code{$new()} will raise an error.
#'
#' Singleton classes are ideal for shared registries, configuration state holders,
#' or runtime controllers that require mutable, centralized coordination logic.
#'
#' Subclasses may define any instance properties needed for their role,
#' but these properties must be assigned before calling \code{super$initialize()},
#' since all bindings will be locked thereafter to ensure shape immutability.
#'
#' @examples
#' # Define a runtime log manager singleton
#' LogManager <- R6::R6Class(
#'   "LogManager",
#'   inherit = RSSingleton,
#'   public = list(
#'     initialize = function() {
#'       self$prefix <- RSString$stringWith("[LOG]")
#'       super$initialize()
#'     },
#'     log = function(msg) {
#'       cat(self$prefix$value(), msg, "\\n")
#'     }
#'   )
#' )
#'
#' # Define singleton accessor
#' LogManager$singleton <- NULL
#' LogManager$instance <- function() {
#'   if (is.null(LogManager$singleton)) LogManager$singleton <- LogManager$new()
#'   LogManager$singleton
#' }
#'
#' # Use the singleton
#' logger <- LogManager$instance()
#' logger$log("System booted.")  # [LOG] System booted.
#'
#' @note
#' The singleton instance is held directly on the class as \code{$singleton}.
#' It is recommended to protect this slot after instantiation using \code{lockBinding("singleton", ClassName)}.
#' Unlike \code{RSModule}, singleton instances are generally mutable.
#' This class does not require \code{private} definitions for instance control.

# REMOVE ON RELEASE
if (exists('register_rs_singleton')) unlockBinding("register_rs_singleton", environment()) # nolint
warning("Reset 'register_rs_singleton'")

register_rs_singleton <- function(klass, obj) {
  klass$.singleton <- obj
  klass$instance <- function() klass$.singleton
  obj$lockVariables()
  lockBinding(".singleton", klass)
  invisible(obj)
}
lockBinding("register_rs_singleton", environment())
RSSingleton <- R6::R6Class(
  "RSSingleton",
  inherit = RSObject,
  cloneable = FALSE,
  public = list(
    # Constructor - must only be called once per singleton
    # All variables must be assigned before calling super$initialize().
    initialize = function() {
      super$initialize()
      klass <- self$class()
      if (!is.null(klass$.singleton)) {
        stop(sprintf("Use %s$instance() instead of new().", klass$classname))
      }
    },
    # Locks all current bindings in the object to prevent accidental additions or removals.
    lockVariables = function() {
      for (name in ls(self, all.names = TRUE)) lockBinding(name, self)
      lockEnvironment(as.environment(self), bindings = TRUE)
    }
  )
)
register_rs_singleton(RSSingleton, RSSingleton$new())