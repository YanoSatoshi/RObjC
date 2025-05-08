#' @title RSCopying: Protocol for Object Copying
#'
#' @description
#' `RSCopying` is a protocol that defines a contract for objects that can be copied
#' into another environment. Conforming objects must implement the method
#' `copyWithEnv(env)`, which produces a deep copy of the object into the specified environment.
#'
#' This protocol is typically used when copying values between container objects
#' (e.g., `RSSet`, `RSDictionary`) or when isolating mutable scope.
#'
#' @section Required Method:
#' - `copyWithEnv(env)` — Returns a copy of the object placed into a specified environment.
#'
#' @section Helper:
#' - `copyObjectWithEnv(copying_obj, env)` — Calls `copyWithEnv()` on a conforming object.
#'
#' @examples
#' \dontrun{
#' MyClass <- R6::R6Class("MyClass", inherit = RSObject, public = list(
#'   value = NULL,
#'   initialize = function(val) { self$value <- val },
#'   copyWithEnv = function(env) {
#'     obj <- MyClass$new(self$value)
#'     environment(obj) <- env
#'     obj
#'   }
#' ))
#' MyClass$protocols <- c("RSCopying")
#' }
#'
#' @export
RSCopying <- R6::R6Class(
  ### protocol name ###
  "RSCopying",
  inherit = RSProtocol,
  cloneable = FALSE,
  public = list(

    #' @description
    #' Lists the method names that any class claiming conformance to this protocol
    #' must implement. Specifically, classes conforming to `RSCopying` are required
    #' to define a method called `mutableCopy`, which returns a (deep) copy of the object.
    #'
    #' @return A character vector of required method names.
    requiredMethods = function() {
      c("copyWithEnv")
    },


    #' @description
    #' Performs a protocol-safe copy of an object into the specified environment.
    #' Verifies that the object conforms to `RSCopying` before calling `copyWithEnv()`.
    #'
    #' @param copying_obj An `RSObject` that implements `copyWithEnv(env)`.
    #' @param env An environment where the copy should reside.
    #' @return A copy of the object within the new environment.
    copyObjectWithEnv = function(copying_obj, env) {
      if (is.null(copying_obj$respondsTo)) {
        stop("Protocol cannot be implimented other than RSObject and its inheritants.") # nolint
      }
      if (!copying_obj$respondsTo("copyWithEnv")) {
        stop(sprintf(
          "Object of class %s does not implement copyWithEnv().",
          copying_obj$class()$classname
        ))
      }
      copying_obj$copyWithEnv(env)
    }
  )
)
register_rs_protocol(RSCopying, RSCopying$new())