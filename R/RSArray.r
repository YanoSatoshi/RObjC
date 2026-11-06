#' @include api_setup.r
#' @include RSMutableCopying.r
#' @include RSCopying.r
#' @include RSComparable.r
#' @include RSListing.r
#' @include RSHashOperator.r
#' @include RSEncoding.r
#'
#' @title RSArray: Immutable, Ordered Object Container
#'
#' @description
#' `RSArray` stores RObjC objects, class generators, and protocol generators
#' in an ordered R list. Its element count, order, and references cannot be
#' changed after construction. A retained mutable element may still change
#' through another reference. Input list names are discarded.
#'
#' Elements are retained by reference. `copy()` returns the receiver;
#' `mutableCopy()` creates a new [RSMutableArray] sharing element objects.
#' Hashes use the standard [RSHashOperator] instance.
#'
#' @section Protocols:
#' - [RSMutableCopying]
#' - [RSCopying]
#' - [RSComparable]
#' - [RSListing]
#' - [RSCoding]
#'
#' @section Core Methods:
#' - `objectAtIndex(index)` — Retrieve an element at a one-based position.
#' - `objectsAtIndexes(indexes)` — Retrieve selected elements as an [RSArray].
#' - `lastObject()` — Retrieve the final element.
#' - `arrayByAddingObject()` and `arrayByAddingObjectsFromArray()` — Create
#'   a new array with additional elements.
#' - `filteredArrayWith()` — Create a new array with selected elements.
#' - `indexOfObject(obj)` and `containsObject(obj)` — Search by equality.
#' - `asList()` — Return the element references in an R list.
#' - `copy()` and `mutableCopy()` — Reuse or copy the container.
#'
#' @export
RSArray <- RSObjectClass(
  "RSArray",
  version = "0.1.1 beta",
  super_class = RSObject,
  protocols = list(RSMutableCopying, RSCopying, RSComparable, RSListing, RSCoding),
  private = list(
    .storage = list(),
    .validElement = function(obj) {
      robjc_isRObjC(obj)
    }
  ),
  public = list(

    #' @description
    #' Constructs a new immutable array instance. Optionally takes a list of RObjC values
    #' to initialize the array.
    #'
    #' @param objects A list of RObjC objects, class generators, or protocol generators
    #'   (default: empty). List names are discarded.
    #' @return A new `RSArray` instance.
    initialize = function(objects = list()) {
      objects <- cast_collections_to_list(objects)
      stopifnot(is.list(objects))
      if (length(objects) > 0 && !all(vapply(objects, private$.validElement, logical(1))))
        stop("All elements must be RObjC objects, classes, or protocols.")
      super$initialize()
      private$.storage <- unname(objects)
      invisible(self)
    },



    #' @description
    #' Returns the number of objects currently stored in the array.
    #'
    #' @return An integer count.
    count = function() length(private$.storage),



    #' @description
    #' Returns the object stored at the specified index.
    #'
    #' @param index One-based numeric position to read; converted with `as.integer()`.
    #' @return The stored RObjC object, class generator, or protocol generator.
    objectAtIndex = function(index) {
      invalid <- find_invalid_indexes(index, length(private$.storage), scalar = TRUE)
      if (length(invalid))
        stop("Index out of bounds: ", toString(invalid), ".", call. = FALSE)
      index <- as.integer(index)
      private$.storage[[index]]
    },



    #' @description Returns selected objects in a new immutable array.
    #'   Indexes are one-based and converted with `as.integer()`. Their order
    #'   and duplicates are preserved; an empty vector yields an empty array.
    #' @param indexes Numeric vector of positions to retrieve.
    #' @return A new [RSArray] sharing the selected element references.
    objectsAtIndexes = function(indexes) {
      invalid <- find_invalid_indexes(indexes, length(private$.storage))
      if (length(invalid))
        stop("Index out of bounds: ", toString(invalid), ".", call. = FALSE)
      RSArray$new(objects = private$.storage[as.integer(indexes)])
    },



    #' @description Returns the last object in the array.
    #' @return The final element reference. An empty array raises an index error.
    lastObject = function() self$objectAtIndex(self$count()),



    #' @description
    #' Checks whether the specified object is present in the array.
    #' Matching is performed using the `isEqual()` method.
    #'
    #' @param obj An RObjC object, class generator, or protocol generator to search for.
    #' @return `TRUE` if the object is found; otherwise `FALSE`.
    containsObject = function(obj) !is.na(self$indexOfObject(obj)),



    #' @description
    #' Returns the 1-based index of the first occurrence of the given object.
    #' Comparison is done via `isEqual()`.
    #'
    #' @param obj An RObjC object, class generator, or protocol generator to locate.
    #' @return An integer index if found; otherwise `as.integer(NA)`.
    indexOfObject = function(obj) {
      idxes <- vapply(
        private$.storage,
        function(x) isTRUE(x$isEqual(obj)),
        logical(1)
      )
      if (any(idxes)) return(which(idxes)[1L]) else NA_integer_
    },



    #' @description
    #' Returns an immutable [RSArray] containing the same element references.
    #'
    #' @return An `RSArray`.
    allObjects = function() self$copy(),



    #' @description
    #' Returns the contents of the array as a standard R list of stored elements.
    #' Elements remain RObjC objects, class generators, or protocol generators.
    #' Used for inspection, serialization, or iteration.
    #'
    #' @return A list of stored elements.
    asList = function() lapply(private$.storage, identity),



    #' @description
    #' Returns the same element references as `asList()` in a standard R list.
    #'
    #' @return A list of stored elements.
    allObjectsAsList = function() self$asList(),



    #' @description Returns the receiver because it is immutable.
    #' @return `self`.
    copy = function() self,



    #' @description
    #' Returns a mutable copy of the array (i.e., an `RSMutableArray` instance).
    #' This performs a shallow copy: the container is recreated,
    #' but the elements are shared as-is without deep duplication.
    #'
    #' It always returns a separate mutable container that shares the original
    #' element references.
    #'
    #' @return An `RSMutableArray` object.
    mutableCopy = function() {
      RSMutableArray$new(objects = private$.storage)
    },



    #' @description Creates an immutable array with one object appended.
    #'   The receiver is unchanged; element references are retained.
    #' @param obj An RObjC object, class generator, or protocol generator.
    #' @return A new [RSArray].
    arrayByAddingObject = function(obj) {
      if (!private$.validElement(obj))
        stop("Element must be an RObjC object, class, or protocol.", call. = FALSE)
      RSArray$new(objects = c(private$.storage, list(obj)))
    },



    #' @description Creates an immutable array by appending another array's
    #'   elements in order. Both source arrays remain unchanged, and element
    #'   references are retained.
    #' @param other An [RSArray] or [RSMutableArray] instance.
    #' @return A new [RSArray].
    arrayByAddingObjectsFromArray = function(other) {
      if (!robjc_isInstance(other) || !other$isKindOf(RSArray))
        stop("`other` must be a kind of RSArray.", call. = FALSE)
      RSArray$new(objects = c(private$.storage, other$asList()))
    },



    #' @description Creates an immutable array containing elements for which
    #'   `lambda` returns one non-missing logical `TRUE`. The predicate is
    #'   called once per element in order. The receiver and its elements are
    #'   unchanged.
    #' @param lambda A function receiving one stored element and returning
    #'   exactly one non-missing logical value.
    #' @return A new [RSArray] of retained element references.
    filteredArrayWith = function(lambda) {
      if (!is.function(lambda))
        stop("`lambda` must be a function.", call. = FALSE)
      keep <- vapply(private$.storage, function(obj) {
        result <- lambda(obj)
        if (!is.logical(result) || length(result) != 1L || is.na(result))
          stop("`lambda` must return one non-missing logical value.", call. = FALSE)
        result
      }, logical(1))
      RSArray$new(objects = private$.storage[keep])
    },



    #' @description Returns the RObjC instance elements for encoding.
    #' Class and protocol generators remain R values and are not traversed as
    #' child objects by the encoder.
    #' @return A list of RObjC instance elements in array order.
    objectsForEncoding = function() Filter(robjc_isInstance, private$.storage),
                                    # An item can be a class or protocol.



    #' @description Encodes the ordered elements. The coder preserves repeated
    #'   element references.
    #' @param coder An [RSKeyedArchiver] recording keyed values.
    #' @return `self`, invisibly.
    encodeWithCoder = function(coder) {
      coder$encodeObjectForKey(private$.storage, "objects")
      invisible(self)
    },



    #' @description Restores the ordered elements without
    #'   calling `initialize()`. This serves both array classes.
    #' @param coder An [RSKeyedUnarchiver] reading keyed values.
    #' @return `self`, invisibly.
    initWithCoder = function(coder) {
      objects <- coder$decodeObjectForKey("objects")
      if (!is.list(objects) || !all(vapply(objects, private$.validElement, logical(1))))
        stop(
          "Decoded array elements must be RObjC objects, classes, or protocols.",
          call. = FALSE
        )
      private$.storage <- unname(objects)
      invisible(self)
    },



    #' @description
    #' Computes an aggregate hash for the array by combining the hash values of all contained elements.
    #' This supports dictionary keys; array ordering uses element comparisons.
    #' The final result passes through [robjc_normalize_hash()].
    #'
    #' @return One non-missing integer hash value.
    hash = function() {
      robjc_normalize_hash(RSHashOperator$instance$combinedHashFromObjects(private$.storage))
    },



    #' @description
    #' Computes a hash code for each object in the array.
    #' Each object must implement `hash()`.
    #'
    #' @return An integer vector of hashes.
    hashesOfObjects = function() RSHashOperator$instance$hashesOfObjects(private$.storage),



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
    #' Compares this array to another for equality.
    #' Two arrays are equal if they have the same length and corresponding
    #' elements are equal according to `isEqual()`.
    #' Mutable and immutable arrays satisfying these conditions are equal.
    #'
    #' @param other Another array object to compare with.
    #' @return `TRUE` if the arrays are equal; otherwise `FALSE`.
    isEqual = function(other) {
      if (!robjc_isInstance(other) || !other$isKindOf(RSArray))
        return(FALSE)
      other_objs <- other$asList()
      if (length(other_objs) != self$count()) return(FALSE)
      all(
        vapply(
          seq_along(private$.storage),
          function(i) isTRUE(private$.storage[[i]]$isEqual(other_objs[[i]])),
          logical(1)
        )
      )
    },



    #' @description
    #' Compares this array to another for ordering.
    #' The comparison is lexicographic. Equal elements are skipped using
    #' `isEqual()`. Differing elements must conform to [RSComparable] and are
    #' ordered using `compare()`; otherwise comparison raises an error. If all
    #' shared elements are equal, the shorter array is considered smaller.
    #'
    #' @param other Another `RSArray` or `RSMutableArray` to compare.
    #' @param na.last Logical; if `TRUE`, NA values are treated as greater.
    #' @return -1 if less than, 0 if equal, 1 if greater.
    compare = function(other, na.last = TRUE) {
      na.last <- tryCatch(as.logical(na.last), error = function(e) NA)
      if (!is.logical(na.last) || length(na.last) != 1L || is.na(na.last))
        stop("`na.last` must be TRUE or FALSE.", call. = FALSE)
      if (!robjc_isInstance(other) || !other$isKindOf(RSArray))
        stop("Cannot compare objects of different class.", call. = FALSE)
      left <- private$.storage
      right <- other$asList()
      for (i in seq_len(min(length(left), length(right)))) {
        if (isTRUE(left[[i]]$isEqual(right[[i]]))) next
        if (!robjc_isInstance(left[[i]]) ||
            !robjc_isInstance(right[[i]]) ||
            !left[[i]]$conformsTo(RSComparable) ||
            !right[[i]]$conformsTo(RSComparable))
          stop("Array elements are not comparable.", call. = FALSE)
        result <- as.integer(sign(left[[i]]$compare(right[[i]], na.last = na.last)))
        if (result != 0L)
          return(result)
      }
      as.integer(sign(length(left) - length(right)))
    }
  )
)
