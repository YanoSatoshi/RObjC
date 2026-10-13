#' @include api_setup.r
#' @include RSMutableCopying.r
#' @include RSCopying.r
#' @include RSHashOperator.r
#' 
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
#' - `copy()` — Return immutable copy (RSArray)
#' - `mutableCopy()` — Return new mutable copy
#'
#' @examples
#' arr <- RSMutableArray$new()
#' arr$addObject(RSString_withValue("a"))
#' arr$insertObjectAtIndex(RSString_withValue("b"), 1)
#' arr$objectAtIndex(1)$value()  # "b"
#'
#' @export
RSMutableArray <- RSObjectClass(
  "RSMutableArray",
  version = "0.0.3 alpha",
  super_class = RSObject,
  protocols   = list(RSMutableCopying, RSCopying),
  private = list(
    .storage = list(),
    .hash_operator = NULL
  ),
  public = list(

    #' @description
    #' Constructs a new mutable array instance. Optionally takes a list of RSObjects
    #' to initialize the array. 
    #'
    #' @param objects A list of `RSObject`s to populate the array (default: empty).
    #' @param hash_operator A module for hash operation.
    #' @return A new `RSMutableArray` instance.
    initialize = function(
      objects = list(),
      hash_operator = RSHashOperator$instance
    ) {
      objects <- cast_rs_storage_to_list(objects)
      stopifnot(is.list(objects))
      super$initialize()
      if (length(objects) > 0 && !all(vapply(objects, robjc_isRobjcObject, logical(1))))
        stop("All elements must inherit from RSObject.")
      private$.storage <- objects
      private$.hash_operator <- hash_operator
      invisible(self)
    },



    #' @description
    #' Appends an object to the end of the array.  
    #'
    #' @param obj An `RSObject`.
    #' @return Invisible `NULL`.
    addObject = function(obj) {
      private$.storage <- c(private$.storage, list(obj))
      invisible(self)
    },



    #' @description
    #' Inserts an object at the specified index, shifting existing elements to the right.  
    #'
    #' @param obj An `RSObject`.
    #' @param index Integer position (1-based) where the object will be inserted.
    #' @return Invisible `NULL`.
    insertObjectAtIndex = function(obj, index) {
      len <- length(private$.storage)
      if (index < 1 || index > len + 1) stop("Index out of bounds.")
      private$.storage <-
        append(private$.storage, list(obj), after = index - 1L)
      invisible(self)
    },



    #' @description
    #' Replaces the object at the specified index with a new one.
    #'
    #' @param obj An `RSObject`.
    #' @param index Integer index (1-based) of the object to replace.
    #' @return Invisible `NULL`.
    replaceObjectAtIndex = function(obj, index) {
      len <- length(private$.storage)
      if (index < 1 || index > len) stop("Index out of bounds.")
      private$.storage[[index]] <- obj
      invisible(self)
    },



    #' @description
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



    #' @description
    #' Clears all objects from the array, resulting in an empty container.
    #'
    #' @return Invisible `NULL`.
    removeAllObjects = function() {
      private$.storage <- list()
      invisible(self)
    },



    #' @description
    #' Returns the object stored at the specified index.
    #'
    #' @param index Integer index (1-based).
    #' @return An `RSObject` stored at the specified position.
    objectAtIndex = function(index) {
      len <- length(private$.storage)
      if (index < 1 || index > len) stop("Index out of bounds.")
      private$.storage[[index]]
    },



    #' @description
    #' Checks whether the specified object is present in the array.
    #' Matching is performed using the `isEqual()` method.
    #'
    #' @param obj An `RSObject` to search for.
    #' @return `TRUE` if the object is found; otherwise `FALSE`.
    containsObject = function(obj) !is.na(self$indexOfObject(obj)),



    #' @description
    #' Returns the 1-based index of the first occurrence of the given object.
    #' Comparison is done via `isEqual()`.
    #'
    #' @param obj An `RSObject` to locate.
    #' @return An integer index if found; otherwise `as.integer(NA)`.
    indexOfObject = function(obj) {
      idxes <- vapply(
        private$.storage,
        function(x) x$isEqual(obj),
        logical(1)
      )
      if (any(idxes)) return(which(idxes)[1L]) else NA_integer_
    },



    #' @description
    #' Returns the full list of stored objects in their internal (possibly cloned) form.
    #' This is a raw accessor used primarily for internal operations.
    #'
    #' @return A list of RS objects.
    allObjects = function() self$copy(),



    #' @description
    #' Returns the contents of the array as a standard R list of RS objects.
    #' Used for inspection, serialization, or iteration.
    #'
    #' @return A list of RS objects.
    asList = function() lapply(private$.storage, identity),



    #' @description
    #' Converts the stored objects into a list of base R values (same with asList()).
    #' Typically used for exporting array contents.
    #'
    #' @return A list of base R values.
    allObjectsAsList = function() self$asList(),



    #' @description
    #' Returns the number of objects currently stored in the array.
    #'
    #' @return An integer count.
    count = function() length(private$.storage),



    #' @description
    #' Returns an immutable copy of the array (i.e., an `RSArray` instance).
    #' This performs a shallow copy: the container is newly created,
    #' but the elements are shared (not deeply duplicated).
    #'
    #' This method is typically used to convert a mutable array to its immutable form.
    #'
    #' @return An `RSArray` object.
    copy = function() {
      RSArray$new(
        objects = private$.storage,
        hash_operator = private$.hash_operator
      )
    },



    #' @description
    #' Returns a mutable copy of the array (i.e., an `RSMutableArray` instance).
    #' This performs a shallow copy: the container is recreated,
    #' but the elements are shared as-is without deep duplication.
    #'
    #' If the original array is already mutable, this method may simply return itself.
    #' 
    #' @return An `RSMutableArray` object.
    mutableCopy = function() {
      RSMutableArray$new(
        objects = private$.storage,
        hash_operator = private$.hash_operator
      )
    },



    #' @description
    #' Computes an aggregate hash for the array by combining the hash values of all contained elements.
    #' This is used for fast comparison and dictionary key support.
    #'
    #' @return An integer hash value.
    hash = function() {
      self$hashOperator()$hashFromObjects(private$.storage)
    },



    #' @description
    #' Computes a hash code for each object in the array.
    #' Each object must implement `hash()`.
    #'
    #' @return An integer vector of hashes.
    hashesOfObjects = function() {
      self$hashOperator()$hashesOfEachObject(private$.storage)
    },



    #' @description
    #' Returns the `RSHashOperator` module instance used for set operations and hashing.
    #' This method provides centralized access to hashing behavior.
    #'
    #' @return The singleton instance of `RSHashOperator`.
    hashOperator = function() {
      private$.hash_operator
    },



    #' @description
    #' Returns a concise textual summary of the data container,
    #' including its class name and the current number of stored objects.
    #' This is primarily used for human-readable identification and logging.
    #'
    #' @return A character string in the form \code{"<ClassName: N objects>"},
    #' where \code{ClassName} is the actual class of the object and \code{N} is the number of elements it contains.
    description = function() {
      sprintf("<%s: %d objects>", self$class()$classname, self$count())
    },


    #' @description
    #' Prints a concise, human-readable summary of the object to the console,
    #' including its class name and element count as returned by `description()`.
    #' Any additional arguments are ignored.
    #'
    #' @param ... Ignored. Included for compatibility with the generic `print()` signature.
    #' @return The object itself, invisibly.
    print = function(...) {
      cat(self$description(), "\n")
      invisible(self)
    },



    #' @description
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



    #' @description
    #' Compares this array to another for ordering.
    #' The comparison is lexicographic: each element is compared in order using `compare()`.
    #' If all elements are equal, the shorter array is considered smaller.
    #'
    #' @param other Another `RSArray` or `RSMutableArray` to compare.
    #' @param na.last Logical; if `TRUE`, NA values are treated as greater.
    #' @return -1 if less than, 0 if equal, 1 if greater.
    compare = function(other, na.last = TRUE) {
      if (!self$isMemberOf(other$class())) {
        stop("Cannot compare objects of different class.")
      }
      h1 <- self$hash()
      h2 <- other$hash()
      na_val <- if (na.last) .Machine$double.xmax else .Machine$double.xmin
      h1 <- if (is.na(h1)) na_val else h1
      h2 <- if (is.na(h2)) na_val else h2
      return(sign(h1 - h2))
    }
  )
)
