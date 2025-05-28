#' @title RSSet: Immutable Hash-Based Set of RSObjects
#'
#' @description
#' `RSSet` is a read-only subclass of `RSMutableSet`. Once created, its elements are
#' flattened and frozen, and any attempt to modify the structure will result in an error.
#'
#' This class provides fast `containsObject()` and `count()` access, and supports copying.
#'
#' @note All mutation methods such as `addObject()` or `removeObject()` are disabled and will raise errors.
#'
#' @export
RSSet <- R6::R6Class(
  "RSSet",
  inherit = RSMutableSet,
  cloneable = FALSE,
  private = list(
    .flat = NULL,
    .freeze = function() {
      if (is.null(private$.bucket)) {
        private$.flat <- list()
      } else {
        flat <- unlist(private$.bucket, recursive = FALSE)
        private$.flat <- flat
      }
      private$.bucket <- NULL
    }
  ),
  public = list(

    #' @description
    #' Constructs a new mutable set.
    #' If a list of `RSObject`s is provided, each object is copied using `copy()`
    #' and stored in the internal hash table as an immutable element, ensuring that the structure  
    #' of the set remains independent from the original input.
    #'
    #' @param objects Optional list of `RSObject`s to initialize the set.
    #' @return A new `RSSet`.
    initialize = function(objects = list()) {
      super$initialize(objects)
      private$.freeze()
      invisible(self)
    },


    #' @description
    #' Checks whether the specified object exists in the set.
    #' Equality is determined using both hash and `isEqual()`.
    #'
    #' @param obj An `RSObject` to check for.
    #' @return `TRUE` if found, otherwise `FALSE`.
    containsObject = function(obj) {
      if (length(private$.flat) == 0) return(FALSE)
      obj_hash <- obj$hash()
      flags <- vapply(
        private$.flat,
        function(x) {x$hash() == obj_hash && x$isEqual(obj)},
        logical(1)
      )
      any(flags)
    },


    #' @description
    #' Returns the total number of stored objects in the set.
    #' @return Integer count.
    count = function() {
      length(private$.flat)
    },



    #' @description
    #' Returns the set's contents as a list of `RSObject` instances.
    #'
    #' @return A list of objects.
    allObjectsAsList = function() {
      private$.flat
    },



    #' @description
    #' Returns the contents of the set as an `RSArray` object.
    #'
    #' @return An `RSArray` containing all objects.
    allObjects = function() {
      RSArray$new(objects = self$allObjectsAsList())
    },



    #' @description
    #' This method is disabled for immutable `RSSet` objects.
    #' Attempting to call this method will result in an error.
    #' @param obj Ignored.
    #' @return No return value. Always raises an error.
    addObject = function(obj) {
      stop(msg_immutable_try_mutate(self$class()))
    },



    #' @description
    #' This method is disabled for immutable `RSSet` objects.
    #' Attempting to call this method will result in an error.
    #' @param obj Ignored.
    #' @return No return value. Always raises an error.
    removeObject = function(obj) {
      stop(msg_immutable_try_mutate(self$class()))
    },



    #' @description
    #' This method is disabled for immutable `RSSet` objects.
    #' Attempting to call this method will result in an error.
    #' @return No return value. Always raises an error.
    removeAllObjects = function() {
      stop(msg_immutable_try_mutate(self$class()))
    },



    #' @description
    #' Returns the object itself, since `RSSet` is immutable.
    #'
    #' This is a no-op copy: the method returns `self`, as immutable
    #' sets can safely be reused without duplication.
    #'
    #' @return The same `RSSet` instance (`self`).
    copy = function() return(self),



    #' @description
    #' Returns a mutable version of the current set.
    #'
    #' This creates a new `RSMutableSet` instance that wraps the same set of elements,
    #' allowing modifications. The copy is shallow: the contained objects are reused
    #' as-is.
    #'
    #' This method is typically used to convert an immutable set into a mutable one
    #' for editing or augmentation.
    #'
    #' @return A new `RSMutableSet` instance.
    mutableCopy = function() RSMutableSet$new(objects = self$allObjectsAsList())
  )
)
