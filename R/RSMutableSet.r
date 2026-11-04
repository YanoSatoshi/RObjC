#' @include RSSet.r
#' @title RSMutableSet: Native-Backed Mutable Set
#'
#' @description
#' `RSMutableSet` extends [RSSet] with operations to add and remove members.
#' It shares the native storage, membership, equality, coding, and set
#' operations of its parent. Inserted objects are retained rather than copied.
#'
#' @section Key Operations:
#' - `addObject()` adds an object if no equal member is present.
#' - `removeObject()` and `removeAllObjects()` remove members.
#' - `copy()` returns an independent immutable [RSSet].
#' - `mutableCopy()` returns an independent mutable set.
#'
#' @export
RSMutableSet <- RSObjectClass(
  "RSMutableSet",
  version = "0.1.1 beta",
  super_class = RSSet,
  protocols = list(RSCopying, RSMutableCopying, RSCoding),
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

    #' @description Returns an immutable shallow copy.
    #' @return A new [RSSet] with independent native storage.
    copy = function() {
      .rs_set_with_storage(RSSet, .rs_set_storage(self)$duplicate())
    }
  )
)
