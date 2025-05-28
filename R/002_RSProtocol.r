#' @title RSProtocol: Abstract Base Class for Method Contracts
#'
#' @description
#' `RSProtocol` is the base class for defining behavioral contracts within the RS framework.
#' Similar to Objective-C's `@protocol`, it specifies method names that a conforming object
#' is expected to implement.
#'
#' This class enforces singleton usage: instantiation via `$new()` is blocked if
#' an instance already exists. Use the class method `$protocol()` instead.
#'
#' Protocol instances are locked after registration to prevent accidental changes to
#' the expected method definitions. This immutability ensures that protocol contracts
#' remain consistent throughout runtime and cannot be redefined dynamically.
#'
#' Subclasses should override `requiredMethods()` to return the character vector of method names.
#' 
#' @section Methods:
#' - `requiredMethods()` — Returns a character vector of method names expected by the protocol.
#' - `lockVariables()` — Locks the internal structure of the protocol instance.
#'
#' @examples
#' \dontrun{
#' MyProtocol <- R6::R6Class(
#'   "MyProtocol",
#'   inherit = RSProtocol,
#'   public = list(
#'     requiredMethods = function() {
#'       c("doSomething", "value")
#'     }
#'   )
#' )
#' register_rs_protocol(MyProtocol, MyProtocol$new())
#' }
#'
#' @export
RSProtocol <- R6::R6Class(
  ### protocol name ###
  "RSProtocol",
  inherit = RSObject,
  cloneable = FALSE,
  public = list(
    #' @description
    #' Constructor - must be called onece per protocol
    #' All properties must be defined before calling super$initialize()
    initialize = function() {
      super$initialize()
      klass <- self$class()
      if (!is.null(klass$robjc_isa$slot)) {
        stop(sprintf("Use %s$protocol() instead of new().", klass$classname))
      }
    },



    #' @description
    #' Returns the list of method names required by this protocol.
    #' Subclasses must override this method to specify which public methods an object
    #' must implement to conform to the protocol.
    #'
    #' If this method is not overridden, it returns an empty character vector and
    #' raises a warning indicating that the protocol definition is incomplete.
    #'
    #' @return A character vector of method names.
    requiredMethods = function() {
      warning(
        sprintf(
          "'requiredMethods' not over-ridden yet for %s.",
          self$class()$classname
        )
      )
      character(0)
    },



    #' @description
    #' Locks all current bindings in the object to prevent accidental additions or removals. #nolint
    lockVariables = function() {
      for (name in ls(self, all.names = TRUE)) lockBinding(name, self)
      lockEnvironment(as.environment(self), bindings = TRUE)
    }
  )
)