#' @include RSSet.r
#' @title RSMutableSet: Native-Backed Mutable Set
#'
#' @description
#' `RSMutableSet` extends [RSSet] with operations to add and remove members.
#' It shares the native storage, membership, equality, coding, and set
#' operations of its parent. Inserted objects are retained rather than copied.
#'
#' @section Protocols:
#' - [RSCopying]
#' - [RSMutableCopying]
#' - [RSListing]
#' - [RSCoding]
#'
#' @section Key Operations:
#' - `addObject()` adds an object if no equal member is present.
#' - `removeObject()` and `removeAllObjects()` remove members.
#' - `uniteSet()`, `intersectSet()`, and `subtractSet()` update membership
#'   using another set.
#' - `copy()` returns an independent immutable [RSSet].
#' - `mutableCopy()` returns an independent mutable set.
#'
#' @export
RSMutableSet <- RSObjectClass(
  "RSMutableSet",
  version = "0.1.1 beta",
  super_class = RSSet,
  protocols = list(RSCopying, RSMutableCopying, RSListing, RSCoding),
  public = list(
    #' @description Constructs a mutable set retaining the supplied objects.
    #' @param objects An optional list or RObjC collection of RObjC objects,
    #'   class generators, or protocol generators. Use `RSNull$null` for a null member.
    #' @return A new `RSMutableSet` instance.
    initialize = function(objects = list()) {
      super$initialize(objects = objects)
      invisible(self)
    },



    #' @description Adds an object if no equal member is present.
    #' @param object An RObjC object, class generator, or protocol generator
    #'   with a valid integer `hash()`. R `NULL` is rejected.
    #' @return `self`, invisibly.
    addObject = function(object) private$.storeObject(object),



    #' @description Removes the member equal to `object`.
    #' @param object An RObjC object, class generator, protocol generator, or
    #'   [RSNull] to remove a null member. R `NULL` is rejected.
    #' @return `TRUE` if a member was removed; otherwise `FALSE`.
    removeObject = function(object) {
      private$.storage$remove(.robjc_collection_object(object))
    },



    #' @description Removes every member.
    #' @return `self`, invisibly.
    removeAllObjects = function() {
      private$.storage$clear()
      invisible(self)
    },



    #' @description Adds every member of `other` to the receiver.
    #' @param other An [RSSet] or [RSMutableSet].
    #' @return `self`, invisibly.
    uniteSet = function(other) {
      .rs_assert_set(other)
      private$.storage$uniteWith(.rs_set_storage(other))
      invisible(self)
    },



    #' @description Adds every member of `other` to the receiver.
    #' @param other An [RSSet] or [RSMutableSet].
    #' @return `self`, invisibly.
    unionSet = function(other) self$uniteSet(other),



    #' @description Retains only members also present in `other`.
    #' @param other An [RSSet] or [RSMutableSet].
    #' @return `self`, invisibly.
    intersectSet = function(other) {
      .rs_assert_set(other)
      private$.storage$intersectWith(.rs_set_storage(other))
      invisible(self)
    },



    #' @description Removes members also present in `other`.
    #' @param other An [RSSet] or [RSMutableSet].
    #' @return `self`, invisibly.
    subtractSet = function(other) {
      .rs_assert_set(other)
      private$.storage$subtractWith(.rs_set_storage(other))
      invisible(self)
    },



    #' @description Removes members also present in `other`.
    #' @param other An [RSSet] or [RSMutableSet].
    #' @return `self`, invisibly.
    minusSet = function(other) self$subtractSet(other),



    #' @description Returns an immutable shallow copy.
    #' @return A new [RSSet] with independent native storage.
    copy = function() {
      .rs_set_with_storage(RSSet, .rs_set_storage(self)$duplicate())
    }
  )
)
