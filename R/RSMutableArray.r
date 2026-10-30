#' @include api_setup.r
#' @include RSMutableCopying.r
#' @include RSCopying.r
#' @include RSComparable.r
#' @include RSHashOperator.r
#' @include RSEncoding.r
#' 
#' @title RSMutableArray: Ordered, Mutable Object Container
#'
#' @description
#' `RSMutableArray` is a dynamically resizable, ordered container that stores
#' RObjC objects, class generators, and protocol generators. It allows indexed access and supports
#' addition, insertion, replacement, and removal of elements.
#' Added, inserted, and replacement elements follow the same value rule.
#' Names on an input list are discarded; elements are addressed by position.
#' A supplied hash operator must hash equal element sequences consistently and
#' remain stable while the array is used as a key or set member.
#'
#' Elements are retained by reference. `copy()` and `mutableCopy()` create new
#' containers that share their element objects with the source array.
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
  version = "0.1.0 beta",
  super_class = RSObject,
  protocols   = list(RSMutableCopying, RSCopying, RSComparable, RSCoding),
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
      robjc_isRobjcObject(obj) || robjc_isClass(obj) || robjc_isProtocol(obj)
    },
    .hash_operator = NULL
  ),
  public = list(

    #' @description
    #' Constructs a new mutable array instance. Optionally takes a list of RObjC values
    #' to initialize the array. 
    #'
    #' @param objects A list of RObjC objects, class generators, or protocol generators
    #'   (default: empty). List names are discarded.
    #' @param hash_operator An RObjC object providing `hashFromObjects()` and
    #'   `hashesOfEachObject()`.
    #' @return A new `RSMutableArray` instance.
    initialize = function(
      objects = list(),
      hash_operator = RSHashOperator$instance
    ) {
      objects <- cast_collections_to_list(objects)
      stopifnot(is.list(objects))
      if (!robjc_isRobjcObject(hash_operator) ||
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
    #' Appends an object to the end of the array.  
    #'
    #' @param obj An RObjC object, class generator, or protocol generator.
    #' @return `self`, invisibly.
    addObject = function(obj) {
      if (!private$.validElement(obj))
        stop("Element must be an RObjC object, class, or protocol.", call. = FALSE)
      private$.storage <- c(private$.storage, list(obj))
      invisible(self)
    },



    #' @description
    #' Inserts an object at the specified index, shifting existing elements to the right.  
    #'
    #' @param obj An RObjC object, class generator, or protocol generator.
    #' @param index One-based, finite whole-number position where the object is inserted.
    #' @return `self`, invisibly.
    insertObjectAtIndex = function(obj, index) {
      index <- private$.validatedIndex(index, length(private$.storage), allow_end = TRUE)
      if (!private$.validElement(obj))
        stop("Element must be an RObjC object, class, or protocol.", call. = FALSE)
      private$.storage <-
        append(private$.storage, list(obj), after = index - 1L)
      invisible(self)
    },



    #' @description
    #' Replaces the object at the specified index with a new one.
    #'
    #' @param obj An RObjC object, class generator, or protocol generator.
    #' @param index One-based, finite whole-number position to replace.
    #' @return `self`, invisibly.
    replaceObjectAtIndex = function(obj, index) {
      index <- private$.validatedIndex(index, length(private$.storage))
      if (!private$.validElement(obj))
        stop("Element must be an RObjC object, class, or protocol.", call. = FALSE)
      private$.storage[[index]] <- obj
      invisible(self)
    },



    #' @description
    #' Removes the object at the given index from the array.
    #' The remaining elements are shifted to fill the gap.
    #'
    #' @param index One-based, finite whole-number position to remove.
    #' @return `self`, invisibly.
    removeObjectAtIndex = function(index) {
      index <- private$.validatedIndex(index, length(private$.storage))
      private$.storage <- private$.storage[-index]
      invisible(self)
    },



    #' @description
    #' Clears all objects from the array, resulting in an empty container.
    #'
    #' @return `self`, invisibly.
    removeAllObjects = function() {
      private$.storage <- list()
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
    objectsForEncoding = function() Filter(robjc_isRobjcObject, private$.storage),


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
    #'   calling `initialize()`. Also serves immutable [RSArray] subclasses.
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
      if (!robjc_isRobjcObject(hash_operator) ||
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
      if (!robjc_isRobjcObject(other) || !other$isKindOf(RSMutableArray))
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
      if (!robjc_isRobjcObject(other) || !other$isKindOf(RSMutableArray))
        stop("Cannot compare objects of different class.", call. = FALSE)
      left <- private$.storage
      right <- other$asList()
      for (i in seq_len(min(length(left), length(right)))) {
        if (isTRUE(left[[i]]$isEqual(right[[i]]))) next
        if (!robjc_isRobjcObject(left[[i]]) ||
            !robjc_isRobjcObject(right[[i]]) ||
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
