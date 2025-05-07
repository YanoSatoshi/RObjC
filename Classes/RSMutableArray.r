#' @title RSMutableArray: Ordered, Mutable Object Container
#'
#' @description
#' `RSMutableArray` is a dynamically resizable, ordered container that stores
#' objects inheriting from `RSObject`. It allows indexed access and supports
#' addition, insertion, replacement, and removal of elements.
#'
#' Elements are deep-copied into the array using the `RSCopying` protocol during insertion,
#' ensuring structural isolation between containers.
#'
#' @section Core Methods:
#' - `addObject(obj)` — Append an object
#' - `insertObjectAtIndex(obj, index)` — Insert at specific position
#' - `replaceObjectAtIndex(obj, index)` — Overwrite an element
#' - `removeObjectAtIndex(index)` — Delete element at index
#' - `removeAllObjects()` — Clear entire array
#' - `objectAtIndex(index)` — Retrieve object by index
#' - `asList()` — Return list of elements
#' - `copyWithEnv(env)` — Return immutable copy (RSArray)
#' - `mutableCopyWithEnv(env)` — Return new mutable copy
#'
#' @examples
#' arr <- RSMutableArray$new()
#' arr$addObject(RSString$stringWith("a"))
#' arr$insertObjectAtIndex(RSString$stringWith("b"), 1)
#' arr$objectAtIndex(1)$value()  # "b"
#'
#' @export
RSMutableArray <- R6::R6Class(
  "RSMutableArray",
  inherit = RSObject,
  cloneable = FALSE,
  private = list(
    .storage = list(),
    .hash_operator = NULL,
    .copying = NULL
  ),
  public = list(

#' @describeIn RSMutableArray
#' Constructs a new mutable array instance. Optionally takes a list of RSObjects
#' to initialize the array. Each object in the list is copied using copyWithEnv(),
#' and stored as an immutable element within the array.
#'
#' @param objects A list of `RSObject`s to populate the array (default: empty).
#' @return A new `RSMutableArray` instance.
    initialize = function(
      objects = list(),
      hash_operator = RSHashOperator$module()
    ) {
      objects <- cast_ry_storage_to_list(objects)
      stopifnot(is.list(objects))
      super$initialize()
      if (
        length(objects) > 0 &&
        !all(vapply( # nolint
          objects,
          function(obj) !is.null(obj$isKindOf) && obj$isKindOf(RSObject),
          logical(1)
        ))
      ) stop("All elements must inherit from RSObject.")
      private$.storage <- objects
      private$.hash_operator <- hash_operator
      private$.copying <- RSCopying$protocol()
      invisible(self)
    },



#' @describeIn RSMutableArray
#' Appends an object to the end of the array.  
#' The object is copied using `copyWithEnv()` and stored as an immutable element,
#' ensuring that the internal structure of the array remains independent from the input object.
#'
#' @param obj An `RSObject` implementing `copyWithEnv(env)`.
#' @return Invisible `NULL`.
    addObject = function(obj) {
      stopifnot(!is.null(obj$isKindOf), obj$isKindOf(RSObject))
      copied <- private$.copying$copyObjectWithEnv(obj, environment(self))
      private$.storage <- c(private$.storage, list(copied))
      invisible(self)
    },



#' @describeIn RSMutableArray
#' Inserts an object at the specified index, shifting existing elements to the right.  
#' The object is copied using `copyWithEnv()` and stored as an immutable element,
#' maintaining isolation from the original input.
#'
#' @param obj An `RSObject` implementing `copyWithEnv(env)`.
#' @param index Integer position (1-based) where the object will be inserted.
#' @return Invisible `NULL`.
    insertObjectAtIndex = function(obj, index) {
      stopifnot(!is.null(obj$isKindOf), obj$isKindOf(RSObject))
      len <- length(private$.storage)
      if (index < 1 || index > len + 1) stop("Index out of bounds.")
      obj <- private$.copying$copyObjectWithEnv(obj, environment(self))
      private$.storage <-
        append(private$.storage, list(obj), after = index - 1L)
      invisible(self)
    },



#' @describeIn RSMutableArray
#' Replaces the object at the specified index with a new one.  
#' The new object is copied using `copyWithEnv()` and stored as an immutable element,
#' replacing the existing content without modifying the input.
#'
#' @param obj An `RSObject` implementing `copyWithEnv(env)`.
#' @param index Integer index (1-based) of the object to replace.
#' @return Invisible `NULL`.
    replaceObjectAtIndex = function(obj, index) {
      stopifnot(!is.null(obj$isKindOf), obj$isKindOf(RSObject))
      len <- length(private$.storage)
      if (index < 1 || index > len) stop("Index out of bounds.")
      obj <- private$.copying$copyObjectWithEnv(obj, environment(self))
      private$.storage[[index]] <- obj
      invisible(self)
    },



#' @describeIn RSMutableArray
#' Removes the object at the given index from the array.
#' The remaining elements are shifted to fill the gap.
#'
#' @param index Integer index (1-based) of the object to remove.
#' @return Invisible `NULL`.
    removeObjectAtIndex = function(index) {
      len <- length(private$.storage)
      if (index < 1 || index > len) stop("Index out of bounds.")
      private$.storage <- private$.storage[-index]
      invisible(self)
    },



#' @describeIn RSMutableArray
#' Clears all objects from the array, resulting in an empty container.
#'
#' @return Invisible `NULL`.
    removeAllObjects = function() {
      private$.storage <- list()
      invisible(self)
    },



#' @describeIn RSMutableArray
#' Returns the object stored at the specified index.
#'
#' @param index Integer index (1-based).
#' @return An `RSObject` stored at the specified position.
    objectAtIndex = function(index) {
      len <- length(private$.storage)
      if (index < 1 || index > len) stop("Index out of bounds.")
      private$.storage[[index]]
    },



#' @describeIn RSMutableArray
#' Checks whether the specified object is present in the array.
#' Matching is performed using the `isEqual()` method.
#'
#' @param obj An `RSObject` to search for.
#' @return `TRUE` if the object is found; otherwise `FALSE`.
    containsObject = function(obj) {
      isTRUE(self$indexOfObject(obj))
    },



#' @describeIn RSMutableArray
#' Returns the 1-based index of the first occurrence of the given object.
#' Comparison is done via `isEqual()`.
#'
#' @param obj An `RSObject` to locate.
#' @return An integer index if found; otherwise `NA_integer_`.
    indexOfObject = function(obj) {
      idxes <- vapply(
        private$.storage,
        function(x) x$isEqual(obj),
        logical(1)
      )
      if (any(idxes)) return(which(idxes)[1L]) else NA_integer
    },



#' @describeIn RSMutableArray
#' Returns the full list of stored objects in their internal (possibly cloned) form.
#' This is a raw accessor used primarily for internal operations.
#'
#' @return A list of RS objects.
    allObjects = function() {
      self$copyWithEnv()
    },



#' @describeIn RSMutableArray
#' Returns the contents of the array as a standard R list of RS objects.
#' Used for inspection, serialization, or iteration.
#'
#' @return A list of RS objects.
    asList = function() {
      lapply(private$.storage, identity)
    },



#' @describeIn RSMutableArray
#' Converts the stored objects into a list of base R values (same with asList()).
#' Typically used for exporting array contents.
#'
#' @return A list of base R values.
    allObjectsAsList = function() {
      self$asList()
    },



#' @describeIn RSMutableArray
#' Returns the number of objects currently stored in the array.
#'
#' @return An integer count.
    count = function() {
      length(private$.storage)
    },



#' @describeIn RSMutableArray
#' Returns an immutable copy of the array (i.e., an `RSArray` instance).
#' Each element is copied via `copyWithEnv()` into the given environment.
#'
#' @param env An environment into which the copy is placed.
#' @return An `RSArray` object.
    copyWithEnv = function(env = parent.frame()) {
      do.call(
        RSArray$new,
        list(
          objects = private$.storage,
          hash_operator = private$.hash_operator
        ),
        envir = env
      )
    },



#' @describeIn RSMutableArray
#' Returns a new mutable copy of the array.
#' Each element is copied into the given environment, preserving the array structure.
#'
#' @param env An environment for evaluating the copy.
#' @return A new `RSMutableArray` object.
    mutableCopyWithEnv = function(env = parent.frame()) {
      if (identical(env, environment(self))) return(self)
      do.call(
        RSMutableArray$new,
        list(
          objects = private$.storage,
          hash_operator = private$.hash_operator
        ),
        envir = env
      )
    },



#' @describeIn RSMutableArray
#' Computes an aggregate hash for the array by combining the hash values of all contained elements.
#' This is used for fast comparison and dictionary key support.
#'
#' @return An integer hash value.
    hash = function() {
      self$hashOperator()$hashFromObjects(private$.storage)
    },



#' @describeIn RSMutableArray
#' Computes a hash code for each object in the array.
#' Each object must implement `hash()`.
#'
#' @return An integer vector of hashes.
    hashesOfObjects = function() {
      self$hashOperator()$hashesOfEachObject(private$.storage)
    },



#' @describeIn RSMutableArray
#' Returns the `RSHashOperator` module instance used for set operations and hashing.
#' This method provides centralized access to hashing behavior.
#'
#' @return The singleton instance of `RSHashOperator`.
    hashOperator = function() {
      private$.hash_operator
    },



    description = function() {
      sprintf("<%s: %d objects>", self$class()$classname, self$count())
    },



    print = function(...) {
      cat(self$description(), "\n")
      invisible(self)
    },



#' @describeIn RSMutableArray
#' Compares this array to another for equality.
#' Two arrays are equal if they have the same length and corresponding elements
#' are equal according to `isEqual()`.
#'
#' @param other Another array object to compare with.
#' @return `TRUE` if the arrays are equal; otherwise `FALSE`.
    isEqual = function(other) {
      if (!other$isMemberOf(self$class())) return(FALSE)
      other_objs <- other$asList()
      if (length(other_objs) != self$count()) return(FALSE)
      all(
        mapply(
          function(x, y) x$isEqual(y),
          private$.storage,
          other_objs,
          USE.NAMES = FALSE
        )
      )
    },



#' @describeIn RSMutableArray
#' Compares this array to another for ordering.
#' The comparison is lexicographic: each element is compared in order using `compare()`.
#' If all elements are equal, the shorter array is considered smaller.
#'
#' @param other Another `RSArray` or `RSMutableArray` to compare.
#' @param na.last Logical; if `TRUE`, NA values are treated as greater.
#' @return -1 if less than, 0 if equal, 1 if greater.
    compare = function(other) {
      if (!self$isMemberOf(other$class())) {
        stop("Cannot compare objects of different class.")
      }
      h1 <- self$hash()
      h2 <- other$hash()
      if (is.na(h1) || is.na(h2)) return(NA_integer_)
      if (h1 < h2) return(-1L)
      if (h1 > h2) return(1L)
      return(0L)
    }
  )
)
