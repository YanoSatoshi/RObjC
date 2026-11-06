#' @include RSArray.r
#' @title RSMutableArray: Ordered, Mutable Object Container
#'
#' @description
#' `RSMutableArray` extends [RSArray] with addition, insertion, replacement,
#' and removal of elements. It shares the parent's list storage, validation,
#' searching, comparison, hashing, and keyed coding methods.
#'
#' Elements are retained by reference. `copy()` creates an independent
#' immutable [RSArray] container with the same element references.
#' `mutableCopy()` creates an independent mutable container.
#'
#' @section Protocols:
#' - [RSMutableCopying]
#' - [RSCopying]
#' - [RSComparable]
#' - [RSListing]
#' - [RSCoding]
#'
#' @section Core Methods:
#' - `addObject(obj)` — Append an object.
#' - `addObjectsFromArray(other)` — Append another array's elements.
#' - `insertObjectAtIndex(obj, index)` — Insert at a one-based position.
#' - `replaceObjectAtIndex(obj, index)` — Replace an element.
#' - `removeObjectAtIndex(index)` and `removeAllObjects()` — Remove elements.
#'
#' @export
RSMutableArray <- RSObjectClass(
  "RSMutableArray",
  version = "0.1.1 beta",
  super_class = RSArray,
  protocols = list(RSMutableCopying, RSCopying, RSComparable, RSListing, RSCoding),
  public = list(
    #' @description Constructs a mutable array from an optional list of RObjC values.
    #' @param objects A list of RObjC objects, class generators, or protocol generators.
    #' @return A new `RSMutableArray` instance.
    initialize = function(objects = list()) {
      super$initialize(objects = objects)
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



    #' @description Appends the elements of another array to the receiver in
    #'   their original order. Elements are retained by reference.
    #' @param other An [RSArray] or [RSMutableArray] instance.
    #' @return `self`, invisibly.
    addObjectsFromArray = function(other) {
      combined <- super$arrayByAddingObjectsFromArray(other)
      private$.storage <- combined$asList()
      invisible(self)
    },



    #' @description
    #' Inserts an object at the specified index, shifting existing elements to the right.  
    #'
    #' @param obj An RObjC object, class generator, or protocol generator.
    #' @param index One-based numeric position where the object is inserted;
    #'   converted with `as.integer()`.
    #' @return `self`, invisibly.
    insertObjectAtIndex = function(obj, index) {
      invalid <- find_invalid_indexes(
        index,
        length(private$.storage),
        allow_end = TRUE,
        scalar = TRUE
      )
      if (length(invalid))
        stop("Index out of bounds: ", toString(invalid), ".", call. = FALSE)
      index <- as.integer(index)
      if (!private$.validElement(obj))
        stop("Element must be an RObjC object, class, or protocol.", call. = FALSE)
      private$.storage <- append(private$.storage, list(obj), after = index - 1L)
      invisible(self)
    },



    #' @description
    #' Replaces the object at the specified index with a new one.
    #'
    #' @param obj An RObjC object, class generator, or protocol generator.
    #' @param index One-based numeric position to replace; converted with `as.integer()`.
    #' @return `self`, invisibly.
    replaceObjectAtIndex = function(obj, index) {
      invalid <- find_invalid_indexes(
        index,
        length(private$.storage),
        scalar = TRUE
      )
      if (length(invalid))
        stop("Index out of bounds: ", toString(invalid), ".", call. = FALSE)
      index <- as.integer(index)
      if (!private$.validElement(obj))
        stop("Element must be an RObjC object, class, or protocol.", call. = FALSE)
      private$.storage[[index]] <- obj
      invisible(self)
    },



    #' @description
    #' Removes the object at the given index from the array.
    #' The remaining elements are shifted to fill the gap.
    #'
    #' @param index One-based numeric position to remove; converted with `as.integer()`.
    #' @return `self`, invisibly.
    removeObjectAtIndex = function(index) {
      invalid <- find_invalid_indexes(
        index,
        length(private$.storage),
        scalar = TRUE
      )
      if (length(invalid))
        stop("Index out of bounds: ", toString(invalid), ".", call. = FALSE)
      index <- as.integer(index)
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



    #' @description Returns an independent immutable shallow copy.
    #' @return A new [RSArray] containing the same element references.
    copy = function() RSArray$new(objects = private$.storage)
  )
)
