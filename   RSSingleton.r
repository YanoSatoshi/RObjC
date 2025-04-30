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
RSSingleton <- R6::R6Class(
  "RSSingleton",
  inherit = RSObject,
  cloneable = FALSE,
  public = list(
    # Constructor - must only be called once per singleton
    # All variables must be assigned before calling super$initialize().
    initialize = function() {
      if (!is.null(RSSingleton$.singleton)) {
        stop(sprintf(
          "Use %s$instance() instead of new().", self$class()$classname
        ))
      }
      super$initialize()
      klass <- self$class()
      klass$instance <- self
      self$lockVariables()
      lockBinding(".singleton", self$class())
    },

    # Locks all current bindings in the object to prevent accidental additions or removals.
    lockVariables = function() {
      for (name in ls(self, all.names = TRUE)) lockBinding(name, self)
      lockEnvironment(as.environment(self), bindings = TRUE)
    }
  )
)

# Singleton instance holder and accessor
RSSingleton$.singleton <- NULL
RSSingleton$instance <- function() {
  if (is.null(RSSingleton$singleton)) RSSingleton$new()
  RSSingleton$singleton
}
