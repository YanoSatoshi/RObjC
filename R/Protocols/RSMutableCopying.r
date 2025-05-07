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
#'
#' @export
RSMutableCopying <- R6::R6Class(
  ### protocol name ###
  "RSMutableCopying",
  inherit = RSProtocol,
  public = list(
    requiredMethods = function() {
      c("mutableCopy")
    },


#' @describeIn RSMutableCopying
#' Validates and performs a mutable copy operation on a conforming object.
#' This method checks whether `mutableCopyWithEnvir()` is implemented before invoking it.
#'
#' @param copying_obj An object implementing `mutableCopyWithEnvir(env)`.
#' @param envir An environment for the copy to be evaluated in.
#' @return A mutable copy of the object.
    mutableCopyObjectWithEnvir = function(copying_obj, envir) {
      if (is.null(copying_obj$respondsTo)) {
        stop("Protocol cannot be implimented other than RSObject and its inheritants.") # nolint
      }
      if (!copying_obj$respondsTo("mutableCopyWithEnvir")) {
        stop(sprintf(
          "Object of class %s does not implement mutableCopyWithEnvir().",
          copying_obj$class()$classname
        ))
      }
    }
  )
)
register_rs_protocol(RSMutableCopying, RSMutableCopying$new())