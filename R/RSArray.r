#' @include RSMutableArray.r
#' @title RSArray: Immutable, Ordered Object Container
#'
#' @description
#' `RSArray` is a read-only subclass of `RSMutableArray`. Once created, its contents
#' cannot be modified. Any attempt to mutate the array will raise an error.
#' Its element count, order, and references cannot be changed after creation.
#' A retained mutable element can still change state through another reference.
#'
#' `$copy()` returns the same immutable array. `$mutableCopy()` creates a new
#' mutable container that shares its element references with the source.
#'
#' @note All mutation methods such as `addObject()`, `removeObjectAtIndex()`, etc.
#' are overridden to raise errors.
#'
#' @examples
#' readonly <- RSArray$new(objects = list(RSString_withValue("X")))
#' readonly$objectAtIndex(1)$value()  # "X"
#' tryCatch(
#'   readonly$addObject(RSString_withValue("Y")),  # Error: immutable
#'   error = function(e) message("Expected error: ", e$message)
#' )
#'
#' @export
RSArray <- RSObjectClass(
  "RSArray",
  version = "0.0.4 alpha",
  super_class = RSMutableArray,
  protocols = list(RSCopying, RSMutableCopying),
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
    #' @param index Ignored.
    #' @return No return value. Always raises an error.
    insertObjectAtIndex = function(obj, index) {
      stop(msg_immutable_try_mutate(self$class()))
    },



    #' @description
    #' This method is disabled for immutable `RSArray` objects. Attempting to call this method will result in an error.
    #' @param obj Ignored.
    #' @param index Ignored.
    #' @return No return value. Always raises an error.
    replaceObjectAtIndex = function(obj, index) {
      stop(msg_immutable_try_mutate(self$class()))
    },



    #' @description
    #' This method is disabled for immutable `RSArray` objects. Attempting to call this method will result in an error.
    #' @param index Ignored.
    #' @return No return value. Always raises an error.
    removeObjectAtIndex = function(index) {
      stop(msg_immutable_try_mutate(self$class()))
    },



    #' @description
    #' This method is disabled for immutable `RSArray` objects. Attempting to call this method will result in an error.
    #' @return No return value. Always raises an error.
    removeAllObjects = function() {
      stop(msg_immutable_try_mutate(self$class()))
    }
  )
)
