#' @title RSSingleton: Base Class for Singleton-Style Managers
#'
#' @description
#' `RSSingleton` is a foundational base class for implementing singleton-style system
#' managers within the RS framework. Unlike `RSModule`, this class is designed
#' for simpler, centralized service objects such as registries, loggers, or runtime controllers.
#'
#' It ensures that only one instance of a subclass is created. Direct instantiation using
#' `$new()` will result in an error. The recommended pattern is to define an
#' explicit accessor method, typically `$instance()`, that internally constructs
#' and stores the singleton instance in `$singleton`.
#'
#' Any instance fields must be assigned before calling `super$initialize()` inside
#' the subclass constructor. After that, all fields will be locked to preserve shape integrity.
#'
#' @examples
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
#' register_rs_singleton(LogManager, LogManager$new())
#'
#' logger <- LogManager$instance()
#' logger$log("System booted.")
#'
#' @note
#' The singleton instance is stored in the class field `singleton`, and should be locked
#' after registration using `register_rs_singleton(SingletonClass, SingletonClass$new())`.
#' The singleton instance is mutable and intended for shared runtime coordination.
#'
#' @export
RSSingleton <- R6::R6Class(
  "RSSingleton",
  inherit = RSObject,
  cloneable = FALSE,
  public = list(

    #' @description
    #' Initializes the singleton instance. Raises an error if a singleton already exists.
    #' This method should be called only once, typically via a custom `$instance()` function.
    initialize = function() {
      super$initialize()
      klass <- self$class()
      if (!is.null(klass$.singleton)) {
        stop(sprintf("Use %s$instance() instead of new().", klass$classname))
      }
    },

    #' @description
    #' Locks all fields and the environment of the object to prevent further changes.
    #' Intended to enforce immutability of the singleton structure.
    lockVariables = function() {
      for (name in ls(self, all.names = TRUE)) lockBinding(name, self)
      lockEnvironment(as.environment(self), bindings = TRUE)
    }
  )
)
register_rs_singleton(RSSingleton, RSSingleton$new())