#' @title RSModule: Base Class for Singleton Utility Modules
#'
#' @description
#' `RSModule` is a structural base class used to define immutable, singleton-accessed modules
#' in the RS framework. It is intended for shared read-only utilities, configurations, or
#' stateless service objects that are initialized once and reused throughout runtime.
#'
#' This class enforces singleton behavior: calling `$new()` directly raises an error if
#' the instance already exists. Subclasses must define their own `$module()` constructor
#' to return the singleton instance.
#'
#' Unlike `RSSingleton`, this class assumes immutability after initialization. All fields
#' must be defined before calling `super$initialize()`, after which all bindings are locked.
#' Subclasses may define public instance properties and methods, but all properties
#' must be defined before calling \code{super$initialize()}, as the module instance will be locked
#' against any further modification or shape changes.
#'
#' @section Usage:
#' - Define a subclass that inherits `RSModule`
#' - Populate instance fields in `initialize()`
#' - Call `super$initialize()` last
#' - Register the module with `register_rs_module()`
#' - Use `$module()` to access the singleton instance
#'
#' @section Methods:
#' - `initialize()` — Guards against double instantiation
#' - `lockVariables()` — Locks all bindings to enforce immutability
#'
#' @examples
#' \dontrun{
#' ConfigModule <- R6::R6Class(
#'   "ConfigModule",
#'   inherit = RSModule,
#'   public = list(
#'     initialize = function() {
#'       self$base_path <- RSString$stringWith("/opt/app/")
#'       super$initialize()
#'     },
#'     getBasePath = function() self$base_path
#'   )
#' )
#' register_rs_module(ConfigModule, ConfigModule$new())
#' # Usage
#' cfg <- ConfigModule$module()
#' cfg$getBasePath()$value()  # "/opt/app/"
#' }
#'
#' @note
#' The singleton instance is stored in the `robjc_isa$slot` field of the class environment,
#' and is automatically locked by \code{register_rs_module()} to prevent reassignment.
#' Users should always access the module via the \code{$module()} method.
#'
#' @export
RSModule <- R6::R6Class(
  "RSModule",
  inherit = RSObject,
  cloneable = FALSE,
  public = list(
    #' @description
    #' Constructor - must be called once per module
    #' All properties must be defined before calling super$initialize().
    initialize = function() {
      super$initialize()
      klass <- self$class()
      if (!is.null(klass$robjc_isa$slot)) {
        stop(sprintf("Use %s$module() instead of new().", klass$classname))
      }
      invisible(self)
    },
    #' @description
    #' Locks all current bindings in the object to prevent accidental additions or removals.
    lockVariables = function() {
      for (name in ls(self, all.names = TRUE)) lockBinding(name, self)
      lockEnvironment(as.environment(self), bindings = TRUE)
    }
  )
)
