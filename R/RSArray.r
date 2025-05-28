#' @title RSArray: Immutable, Ordered Object Container
#'
#' @description
#' `RSArray` is a read-only subclass of `RSMutableArray`. Once created, its contents
#' cannot be modified. Any attempt to mutate the array will raise an error.
#' This class is suitable for safely sharing ordered collections without risk of modification.
#'
#' Use `$copy()` to duplicate the array as-is, or `$mutableCopy()` to obtain
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
    #' Returns the object itself, since `RSArray` is immutable.
    #' 
    #' This is a no-op copy: the method simply returns `self`,
    #' as immutable objects are safe to reuse without duplication.
    #'
    #' @return The same `RSArray` instance (`self`).
    copy = function() return(self),



    #' @description
    #' Returns a mutable version of the current array.
    #' 
    #' This performs a shallow copy of the array elements and wraps them
    #' in a new `RSMutableArray` instance, allowing modifications to the content.
    #'
    #' This is typically used when an immutable array needs to be transitioned into
    #' a mutable one for editing.
    #'
    #' @return A new `RSMutableArray` instance.
    mutableCopy = function() RSMutableArray$new(
      objects = private$.storage,
      hash_operator = private$.hash_operator
    ),



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
