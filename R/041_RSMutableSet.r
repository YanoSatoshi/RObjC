#' @title RSMutableSet: Hash-Based Mutable Set of Unique RSObjects
#'
#' @description
#' `RSMutableSet` is a mutable, unordered collection of unique `RSObject` instances.
#' It uses `RSDynamicHash` internally to guarantee hash-based uniqueness,
#' with support for insertion, removal, set operations, and object comparison.
#'
#' Elements are deep-copied into the set using the `copy()` method of the `RSCopying` protocol.
#'
#' @section Key Operations:
#' - `addObject()`, `containsObject()`, `removeObject()`, `removeAllObjects()`
#' - `unionSet()`, `intersectSet()`, `differenceSet()`
#' - `copy()` / `mutableCopy()`
#'
#' @export
RSMutableSet <- R6::R6Class(
  "RSMutableSet",
  inherit = RSDynamicHash,
  cloneable = FALSE,
  public = list(

    #' @description
    #' Constructs a new mutable set.
    #' If a list of `RSObject`s is provided, each object is copied using `copy()`
    #' and stored in the internal hash table as an immutable element, ensuring
    #' that the structure of the set remains independent from the original input.
    #'
    #' @param objects Optional list of `RSObject`s to initialize the set.
    #' @return A new `RSMutableSet`.
    initialize = function(objects = list()) {
      objects <- cast_rs_storage_to_list(objects)
      if (
        length(objects) > 0 &&
        !all(vapply(
          objects,
          function(obj) !is.null(obj$isKindOf) && obj$isKindOf(RSObject),
          logical(1)
        ))
      ) stop("All elements must inherit from RSObject.")
      super$initialize(objects = lapply(objects, function(obj) obj$copy()))
      invisible(self)
    },



    #' @description
    #' Adds a new object to the set. The object is copied using `copy()` and
    #' stored in the hash table. Duplicate objects (as defined by `isEqual()`)
    #' will not be added.
    #'
    #' @param obj An `RSObject` to insert.
    #' @return Invisible `NULL`.
    addObject = function(obj) {
      super$setObject(obj$copy())
    },



    #' @description
    #' Checks whether the specified object exists in the set.
    #' Equality is determined using both hash and `isEqual()`.
    #'
    #' @param obj An `RSObject` to check for.
    #' @return `TRUE` if found, otherwise `FALSE`.
    containsObject = function(obj) {
      !is.null(super$getObject(obj))
    },


    #' @description
    #' Returns the set's contents as a list of `RSObject` instances.
    #'
    #' @return A list of objects.
    allObjectsAsList = function() super$allObjectsAsList(),



    #' @description
    #' Returns the contents of the set as an `RSArray` object.
    #'
    #' @return An `RSArray` containing all objects.
    allObjects = function() RSArray$new(self$allObjectsAsList()),



    #' @description
    #' Returns the set's contents as a list of `RSObject` instances.
    #'
    #' @return A list of objects.
    asList = function() super$allObjectsAsList(),



    #' @description
    #' Returns the union of this set and another `RSMutableSet`.
    #' The result is returned as a new `RSSet` instance.
    #'
    #' @param other Another `RSMutableSet`.
    #' @return An `RSSet` representing the union.
    unionSet = function(other) {
      stopifnot(other$isKindOf(RSMutableSet))
      union <- self$hash_operator$unionOfSets(
        self$allObjectsAsList(), other$allObjectsAsList()
      )
      RSSet$new(union)
    },



    #' @description
    #' Returns the intersection of this set and another `RSMutableSet`.
    #' The result is returned as a new `RSSet` instance.
    #'
    #' @param other Another `RSMutableSet`.
    #' @return An `RSSet` containing common objects.
    intersectSet = function(other) {
      stopifnot(other$isKindOf(RSMutableSet))
      intersection <- self$hash_operator$intersectionOfSets(
        self$allObjectsAsList(),
        other$allObjectsAsList()
      )
      RSSet$new(intersection)
    },



    #' @description
    #' Returns the difference between this set and another `RSMutableSet`.
    #' The result is returned as a new `RSSet` instance.
    #'
    #' @param other Another `RSMutableSet`.
    #' @return An `RSSet` of elements in this set but not in the other.
    differenceSet = function(other) {
      stopifnot(other$isKindOf(RSMutableSet))
      difference <- self$hash_operator$differenceOfSets(
        self$allObjectsAsList(),
        other$allObjectsAsList()
      )
      RSSet$new(difference)
    },



    #' @description
    #' Returns an immutable copy (`RSSet`) of the current set.
    #' 
    #' This performs a shallow copy: elements are preserved as-is,
    #' and a new `RSSet` instance is created to wrap them.
    #' 
    #' Use this method to obtain a non-modifiable version of the current set.
    #'
    #' @return An `RSSet` copy of the set.
    copy = function() RSSet$new(objects = self$allObjectsAsList()),


    #' @description
    #' Returns a mutable copy (`RSMutableSet`) of the current set.
    #' 
    #' This performs a shallow copy: the element list is reused,
    #' but a new mutable set wrapper is created. If the instance is already mutable,
    #' returning `self` may be acceptable depending on usage context.
    #'
    #' @return A `RSMutableSet` copy.
    mutableCopy = function() RSMutableSet$new(objects = self$allObjectsAsList())
  )
)
conform_protocol_to_class(RSMutableCopying$protocol(), RSMutableSet)
conform_protocol_to_class(RSCopying$protocol(), RSMutableSet)