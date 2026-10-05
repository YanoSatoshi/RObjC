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
#' After registration via `register_rs_singleton()`, the singleton instance can be optionally
#' locked using `lockVariables()` to prevent modification. This protects runtime stability
#' by ensuring the structure of shared global state cannot change unexpectedly.
#'
#' Any instance fields must be assigned before calling `super$initialize()` inside
#' the subclass constructor. After that, all fields will be locked to preserve shape integrity.
#'
#' @examples
#' \dontrun{
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
#' }
#'
#' @note
#' The singleton instance is stored in the class field `robjc_isa$slot`, and may be locked
#' after registration using `register_rs_singleton(SingletonClass, SingletonClass$new())`
#' with `lock_all = TRUE` (the default).
#' Locking ensures that the shared singleton instance remains structurally stable throughout runtime.
#' You may disable locking during testing or controlled reconfiguration by passing `lock_all = FALSE`.
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
      if (!is.null(klass$robjc_isa$slot)) {
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
