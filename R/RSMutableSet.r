#' @include api_setup.r
#' @include RSConcreteSet.r
#' @include RSCopying.r
#' @include RSMutableCopying.r
#' 
#' @title RSMutableSet: Native-Backed Mutable Set
#'
#' @description
#' `RSMutableSet` is a mutable, unordered collection of unique RObjC objects,
#' class generators, and protocol generators. Storage is delegated to the internal [RSConcreteSet] adapter,
#' which uses each member's `hash()` and `isEqual()` methods.
#'
#' As with Foundation sets, inserted objects are retained rather than copied.
#' Consequently, a member's `hash()` and equality-relevant state must not change
#' while it belongs to a set. `hash()` is checked before native insertion and
#' must return one nonmissing integer. R `NULL` is represented by [RSNull].
#'
#' Set operations return new immutable [RSSet] objects and leave both operands
#' unchanged.
#'
#' @section Key Operations:
#' - `addObject()`, `member()`, `containsObject()`, and `anyObject()`.
#' - `removeObject()` and `removeAllObjects()`.
#' - `uniteSet()`, `intersectSet()`, and `subtractSet()`.
#' - `copy()` and `mutableCopy()`.
#'
#' @export
RSMutableSet <- RSObjectClass(
  "RSMutableSet",
  version = "0.0.3 alpha",
  super_class = RSObject,
  protocols   = list(RSCopying, RSMutableCopying),
  private = list(
    .storage = NULL,

    .wrapNull = function(object) {
      if (is.null(object)) return(RSNull$null)
      if (robjc_isRobjcObject(object) || robjc_isMetaClass(object)) return(object)
      stop(sprintf("Unsupported object type: %s", class(object)[1]))
    },

    .storeObject = function(item) {
      item <- private$.wrapNull(item)
      hash <- item$hash()
      if (!is.integer(hash) || length(hash) != 1L || is.na(hash))
        stop("Member hash() must return one non-missing integer.", call. = FALSE)
      private$.storage$add(item)
      invisible(self)
    }
  ),
  public = list(
    #' @description Constructs a mutable set retaining the supplied objects.
    #' @param objects An optional list or RObjC collection of RObjC objects,
    #'   class generators, or protocol generators. `NULL`
    #'   elements are represented by [RSNull].
    #' @return A new `RSMutableSet` instance.
    initialize = function(objects = list()) {
      objects <- cast_collections_to_list(objects)
      valid <- vapply(
        objects,
        function(object) is.null(object) || robjc_isRobjcObject(object) ||
          robjc_isMetaClass(object),
        logical(1)
      )
      if (!all(valid)) stop("All elements must be RObjC objects, classes, protocols, or NULL.")

      super$initialize()
      private$.storage <- RSConcreteSet$new()
      for (item in objects) private$.storeObject(item)
      invisible(self)
    },

    #' @description Adds an object if no equal member is present.
    #' @param object An RObjC object, class generator, or protocol generator
    #'   with a valid integer `hash()`, or `NULL` to add [RSNull].
    #' @return `self`, invisibly.
    addObject = function(object) private$.storeObject(object),

    #' @description Returns the number of members.
    #' @return A non-negative numeric scalar.
    count = function() private$.storage$count(),

    #' @description Returns the stored member equal to `object`.
    #' @param object An RObjC object, class generator, protocol generator, or
    #'   `NULL` to look up [RSNull].
    #' @return The stored member, or `NULL` when absent.
    member = function(object) {
      private$.storage$member(private$.wrapNull(object))
    },

    #' @description Tests whether an equal member is present.
    #' @param object An RObjC object, class generator, protocol generator, or
    #'   `NULL` to look up [RSNull].
    #' @return A logical scalar.
    containsObject = function(object) {
      private$.storage$contains(private$.wrapNull(object))
    },

    #' @description Returns an arbitrary stored member.
    #' @return A stored member, or `NULL` when the set is empty. No ordering is
    #'   implied and the selected member may change after mutation.
    anyObject = function() private$.storage$anyObject(),

    #' @description Returns all members as a list.
    #' @return A list in unspecified order.
    allObjectsAsList = function() private$.storage$allObjects(),

    #' @description Returns all members as a list.
    #' @return A list in unspecified order.
    asList = function() self$allObjectsAsList(),

    #' @description Returns all members as an [RSArray].
    #' @return An `RSArray` in unspecified order.
    allObjects = function() RSArray$new(self$allObjectsAsList()),

    #' @description Removes the member equal to `object`.
    #' @param object An RObjC object, class generator, protocol generator, or
    #'   `NULL` to remove [RSNull].
    #' @return `TRUE` if a member was removed; otherwise `FALSE`.
    removeObject = function(object) {
      private$.storage$remove(private$.wrapNull(object))
    },

    #' @description Removes every member.
    #' @return `self`, invisibly.
    removeAllObjects = function() {
      private$.storage$clear()
      invisible(self)
    },

    #' @description Computes an order-independent aggregate member hash.
    #' @return An integer hash value.
    hash = function() private$.storage$contentHash(),

    #' @description Compares two sets by membership.
    #' @param other Another set.
    #' @return `TRUE` when both sets contain equal members.
    isEqual = function(other) {
      if (!robjc_isRobjcObject(other) ||
          !other$isKindOf(RSMutableSet) ||
          self$count() != other$count()) {
        return(FALSE)
      }
      all(vapply(
        self$allObjectsAsList(),
        function(item) isTRUE(other$containsObject(item)),
        logical(1)
      ))
    },

    #' @description Returns the RObjC instance members required for encoding.
    #'   Class and protocol generators are serialized as R values.
    #' @return A list of RObjC instance members.
    objectsForEncoding = function() Filter(robjc_isRobjcObject, self$allObjectsAsList()),

    #' @description Captures native storage for R serialization.
    #' @return `self`, invisibly.
    prepareForSerialization = function() {
      private$.storage$prepareForSerialization()
      invisible(self)
    },

    #' @description Returns the union with `other` without changing either set.
    #' @param other Another mutable or immutable set.
    #' @return A new [RSSet].
    uniteSet = function(other) {
      .rs_assert_set(other)
      result <- private$.storage$duplicate()
      result$uniteWith(.rs_set_storage(other))
      .rs_set_with_storage(RSSet, result)
    },

    #' @description Returns the intersection with `other` without changing either set.
    #' @param other Another mutable or immutable set.
    #' @return A new [RSSet].
    intersectSet = function(other) {
      .rs_assert_set(other)
      result <- private$.storage$duplicate()
      result$intersectWith(.rs_set_storage(other))
      .rs_set_with_storage(RSSet, result)
    },

    #' @description Returns the members not present in `other`.
    #' @param other Another mutable or immutable set.
    #' @return A new [RSSet].
    subtractSet = function(other) {
      .rs_assert_set(other)
      result <- private$.storage$duplicate()
      result$subtractWith(.rs_set_storage(other))
      .rs_set_with_storage(RSSet, result)
    },

    #' @description Returns an immutable shallow copy.
    #' @return A new [RSSet] with independent native storage.
    copy = function() {
      .rs_set_with_storage(
        RSSet,
        .rs_set_storage(self)$duplicate()
      )
    },

    #' @description Returns an independent mutable shallow copy.
    #' @return A new `RSMutableSet`.
    mutableCopy = function() {
      .rs_set_with_storage(
        RSMutableSet,
        .rs_set_storage(self)$duplicate()
      )
    }
  )
)

.rs_assert_set <- function(set) {
  stopifnot(
    "Object must be a set" =
      robjc_isRobjcObject(set) && set$isKindOf(RSMutableSet)
  )
  invisible(TRUE)
}

.rs_set_storage <- function(set) {
  .rs_assert_set(set)
  set$.__enclos_env__$private$.storage
}

.rs_set_with_storage <- function(generator, storage) {
  stopifnot(inherits(storage, "RSConcreteSet"))
  result <- generator$new()
  result$.__enclos_env__$private$.storage <- storage
  result
}
