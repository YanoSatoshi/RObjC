#' @include api_setup.r
#' @include RSMutableCopying.r
#' @include RSCopying.r
#' @include RSComparable.r
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
#' A supplied hash operator must hash equal element sequences consistently
#' and remain stable while the array is used as a key or set member.
#'
#' @section Core Methods:
#' - `objectAtIndex(index)` — Retrieve an element at a one-based position.
#' - `indexOfObject(obj)` and `containsObject(obj)` — Search by equality.
#' - `asList()` — Return the element references in an R list.
#' - `copy()` and `mutableCopy()` — Reuse or copy the container.
#'
#' @export
RSArray <- RSObjectClass(
  "RSArray",
  version = "0.1.1 beta",
  super_class = RSObject,
  protocols = list(RSMutableCopying, RSCopying, RSComparable, RSCoding),
  private = list(
    .storage = list(),
    .validatedIndex = function(index, len, allow_end = FALSE) {
      if (!is.numeric(index) || length(index) != 1L || !is.finite(index) ||
          index != trunc(index) || index < 1 ||
          index > len + as.integer(allow_end))
        stop("Index out of bounds or not a whole number.", call. = FALSE)
      index
    },
    .validElement = function(obj) {
      robjc_isInstance(obj) || robjc_isClass(obj) || robjc_isProtocol(obj)
    },
    .hash_operator = NULL
  ),
  public = list(

    #' @description
    #' Constructs a new immutable array instance. Optionally takes a list of RObjC values
    #' to initialize the array.
    #'
    #' @param objects A list of RObjC objects, class generators, or protocol generators
    #'   (default: empty). List names are discarded.
    #' @param hash_operator An RObjC object providing `hashFromObjects()` and
    #'   `hashesOfEachObject()`.
    #' @return A new `RSArray` instance.
    initialize = function(
      objects = list(),
      hash_operator = RSHashOperator$instance
    ) {
      objects <- cast_collections_to_list(objects)
      stopifnot(is.list(objects))
      if (!robjc_isInstance(hash_operator) ||
          !hash_operator$respondsTo("hashFromObjects") ||
          !hash_operator$respondsTo("hashesOfEachObject")) {
        stop("`hash_operator` must provide `hashFromObjects()` and `hashesOfEachObject()`.",
             call. = FALSE)
      }
      super$initialize()
      if (length(objects) > 0 && !all(vapply(objects, private$.validElement, logical(1))))
        stop("All elements must be RObjC objects, classes, or protocols.")
      private$.storage <- unname(objects)
      private$.hash_operator <- hash_operator
      invisible(self)
    },



    #' @description
    #' Returns the object stored at the specified index.
    #'
    #' @param index One-based, finite whole-number position to read.
    #' @return The stored RObjC object, class generator, or protocol generator.
    objectAtIndex = function(index) {
      index <- private$.validatedIndex(index, length(private$.storage))
      private$.storage[[index]]
    },



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



    #' @description Returns the RObjC instance elements for encoding.
    #' Class and protocol generators remain R values and are not traversed as
    #' child objects by the encoder.
    #' @return A list of RObjC instance elements in array order.
    objectsForEncoding = function() Filter(robjc_isInstance, private$.storage),


    #' @description Encodes the ordered elements and, when non-default, the
    #'   hash operator. The coder preserves repeated element references.
    #' @param coder An [RSKeyedArchiver] recording keyed values.
    #' @return `self`, invisibly.
    encodeWithCoder = function(coder) {
      coder$encodeObjectForKey(private$.storage, "objects")
      if (!identical(private$.hash_operator, RSHashOperator$instance))
        coder$encodeObjectForKey(private$.hash_operator, "hash_operator")
      invisible(self)
    },


    #' @description Restores the ordered elements and hash operator without
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
      hash_operator <- coder$decodeObjectForKey("hash_operator")
      if (is.null(hash_operator)) hash_operator <- RSHashOperator$instance
      if (!robjc_isInstance(hash_operator) ||
          !hash_operator$respondsTo("hashFromObjects") ||
          !hash_operator$respondsTo("hashesOfEachObject"))
        stop("Decoded hash operator is invalid.", call. = FALSE)
      private$.storage <- unname(objects)
      private$.hash_operator <- hash_operator
      invisible(self)
    },



    #' @description
    #' Returns the number of objects currently stored in the array.
    #'
    #' @return An integer count.
    count = function() length(private$.storage),



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
      RSMutableArray$new(
        objects = private$.storage,
        hash_operator = private$.hash_operator
      )
    },



    #' @description
    #' Computes an aggregate hash for the array by combining the hash values of all contained elements.
    #' This supports dictionary keys; array ordering uses element comparisons.
    #' The final result passes through [robjc_normalize_hash()].
    #'
    #' @return One non-missing integer hash value.
    hash = function() {
      robjc_normalize_hash(self$hashOperator()$hashFromObjects(private$.storage))
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
    #' Returns the hash operator supplied at initialization, or the default
    #' [RSHashOperator] instance when none was supplied.
    #'
    #' @return The stored hash operator. The default is `RSHashOperator$instance`.
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
    #' Two arrays are equal if they use the same hash operator instance, have the
    #' same length, and corresponding elements are equal according to `isEqual()`.
    #' Mutable and immutable arrays satisfying these conditions are equal.
    #'
    #' @param other Another array object to compare with.
    #' @return `TRUE` if the arrays are equal; otherwise `FALSE`.
    isEqual = function(other) {
      if (!robjc_isInstance(other) || !other$isKindOf(RSArray))
        return(FALSE)
      if (!identical(private$.hash_operator, other$hashOperator()))
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
