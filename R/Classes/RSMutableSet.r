#' @title RSMutableSet: Hash-Based Mutable Set of Unique RSObjects
#'
#' @description
#' `RSMutableSet` is a mutable, unordered collection of unique `RSObject` instances.
#' It uses `RSDynamicHash` internally to guarantee hash-based uniqueness,
#' with support for insertion, removal, set operations, and object comparison.
#'
#' Elements are deep-copied into the set using the `copyWithEnv()` method of the `RSCopying` protocol.
#'
#' @section Key Operations:
#' - `addObject()`, `containsObject()`, `removeObject()`, `removeAllObjects()`
#' - `unionSet()`, `intersectSet()`, `differenceSet()`
#' - `copyWithEnv()` / `mutableCopyWithEnv()`
#'
#' @export
RSMutableSet <- R6::R6Class(
  "RSMutableSet",
  inherit = RSDynamicHash,
  cloneable = FALSE,
  private = list(
    .copying = NULL
  ),
  public = list(

#' @describeIn RSMutableSet
#' Constructs a new mutable set.
#' If a list of `RSObject`s is provided, each object is copied using `copyWithEnv()`
#' and stored in the internal hash table as an immutable element, ensuring that the structure  
#' of the set remains independent from the original input.
#'
#' @param objects Optional list of `RSObject`s to initialize the set.
#' @return A new `RSMutableSet`.
    initialize = function(objects = list()) {
      objects <- cast_ry_storage_to_list(objects)
      if (
        length(objects) > 0 &&
        !all(vapply(
          objects,
          function(obj) !is.null(obj$isKindOf) && obj$isKindOf(RSObject),
          logical(1)
        ))
      ) stop("All elements must inherit from RSObject.")
      super$initialize(objects = objects)
      private$.copying <- RSCopying$protocol()
      invisible(self)
    },



#' @describeIn RSMutableSet
#' Adds a new object to the set. The object is copied using `copyWithEnv()` and stored in the hash table.
#' Duplicate objects (as defined by `isEqual()`) will not be added.
#'
#' @param obj An `RSObject` to insert.
#' @return Invisible `NULL`.
    addObject = function(obj) {
      stopifnot(!is.null(obj$isKindOf), obj$isKindOf(RSObject))
      copied <- private$.copying$copyObjectWithEnv(obj, environment(self))
      super$setObject(copied)
    },



#' @describeIn RSMutableSet
#' Checks whether the specified object exists in the set.
#' Equality is determined using both hash and `isEqual()`.
#'
#' @param obj An `RSObject` to check for.
#' @return `TRUE` if found, otherwise `FALSE`.
    containsObject = function(obj) {
      !is.null(super$getObject(obj))
    },


#' @describeIn RSMutableSet
#' Returns the set's contents as a list of `RSObject` instances.
#'
#' @return A list of objects.
    allObjectsAsList = function() {
      super$allObjectsAsList()
    },



#' @describeIn RSMutableSet
#' Returns the contents of the set as an `RSArray` object.
#'
#' @return An `RSArray` containing all objects.
    allObjects = function() {
      RSArray$new(self$allObjectsAsList())
    },



#' @describeIn RSMutableSet
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



#' @describeIn RSMutableSet
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



#' @describeIn RSMutableSet
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



#' @describeIn RSMutableSet
#' Returns an immutable copy (`RSSet`) of the current set.
#' Each element is duplicated using `copyWithEnv()` into the target environment.
#'
#' @param env Environment to evaluate the copy in.
#' @return An `RSSet` copy of the set.
    copyWithEnv = function(env = parent.frame()) {
      do.call(
        RSSet$new,
        list(objects = self$allObjectsAsList()),
        envir = env
      )
    },



#' @describeIn RSMutableSet
#' Returns a new mutable copy of the current set.
#' All elements are duplicated into the target environment.
#'
#' @param env Environment for the new instance.
#' @return A `RSMutableSet` copy.
    mutableCopyWithEnv = function(env = parent.frame()) {
      if (identical(env, environment(self))) return(self)
      do.call(
        RSMutableSet$new,
        list(objects = self$allObjectsAsList()),
        envir = env
      )
    }
  )
)
