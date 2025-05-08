#' @title RSArray: Immutable, Ordered Object Container
#'
#' @description
#' `RSArray` is a read-only subclass of `RSMutableArray`. Once created, its contents
#' cannot be modified. Any attempt to mutate the array will raise an error.
#' This class is suitable for safely sharing ordered collections without risk of modification.
#'
#' Use `$copyWithEnv()` to duplicate the array as-is, or `$mutableCopyWithEnv()` to obtain
#' a mutable version.
#'
#' @note All mutation methods such as `addObject()`, `removeObjectAtIndex()`, etc.
#' are overridden to raise errors.
#'
#' @examples
#' readonly <- RSArray$new(objects = list(RSString$stringWith("X")))
#' readonly$objectAtIndex(1)$value()  # "X"
#' tryCatch(
#'   readonly$addObject(RSString$stringWith("Y")),  # Error: immutable
#'   error = function(e) message("Expected error: ", e$message)
#' )
#'
#' @export
RSArray <- R6::R6Class(
  "RSArray",
  inherit = RSMutableArray,
  cloneable = FALSE,
  public = list(

    #' @description
    #' Returns an identical immutable copy of the current object placed into the specified environment.
    #'   Because the object is already immutable, this method typically returns `self` unless an environment
    #'   switch is needed for structural consistency.
    #' @param env The target environment.
    #' @return A copy of the current immutable object, typically `self`.
    copyWithEnv = function(env = parent.frame()) {
      if (identical(env, environment(self))) return(self)
      do.call(
        RSArray$new,
        list(
          objects = private$.storage,
          hash_operator = private$.hash_operator
        ),
        envir = env
      )
    },



    #' @description
    #' Returns a new mutable version of the current object in the specified environment.
    #'   The content is duplicated to allow modifications, and the result is typically
    #'   an instance of the corresponding mutable subclass (e.g., `RSMutableArray` for `RSArray`).
    #' @param env The environment in which the new mutable object will be evaluated.
    #' @return A new mutable copy of the object.
    mutableCopyWithEnv = function(env = parent.frame()) {
      do.call(
        RSMutableArray$new,
        list(
          objects = private$.storage,
          hash_operator = private$.hash_operator
        ),
        envir = env
      )
    },



    #' @description
    #' This method is disabled for immutable `RSArray` objects. Attempting to call this method will result in an error.
    #' @param obj Ignored.
    #' @return No return value. Always raises an error.
    addObject = function(obj) {
      stop(msg_immutable_try_mutate(self$class()))
    },



    #' @description
    #' This method is disabled for immutable `RSArray` objects. Attempting to call this method will result in an error.
    #' @param obj Ignored.
    #' @param idx Ignored.
    #' @return No return value. Always raises an error.
    insertObjectAtIndex = function(obj, idx) {
      stop(msg_immutable_try_mutate(self$class()))
    },



    #' @description
    #' This method is disabled for immutable `RSArray` objects. Attempting to call this method will result in an error.
    #' @param obj Ignored.
    #' @param idx Ignored.
    #' @return No return value. Always raises an error.
    replaceObjectAtIndex = function(obj, idx) {
      stop(msg_immutable_try_mutate(self$class()))
    },



    #' @description
    #' This method is disabled for immutable `RSArray` objects. Attempting to call this method will result in an error.
    #' @param obj Ignored.
    #' @param idx Ignored.
    #' @return No return value. Always raises an error.
    removeObjectAtIndex = function(obj, idx) {
      stop(msg_immutable_try_mutate(self$class()))
    },



    #' @description
    #' This method is disabled for immutable `RSArray` objects. Attempting to call this method will result in an error.
    #' @param obj Ignored.
    #' @return No return value. Always raises an error.
    removeAllObjects = function(obj) {
      stop(msg_immutable_try_mutate(self$class()))
    }
  )
)
