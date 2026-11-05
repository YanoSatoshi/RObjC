#' @include api_setup.r
#' @include RSConcreteSet.r
#' @include RSCopying.r
#' @include RSMutableCopying.r
#' @include RSEncoding.r
#'
#' @title RSSet: Native-Backed Immutable Set
#'
#' @description
#' `RSSet` is an immutable, unordered collection of unique RObjC objects,
#' class generators, and protocol generators. [RSConcreteSet] provides native
#' storage and uses each member's `hash()` and `isEqual()` methods.
#'
#' Inserted objects are retained rather than copied. A member's hash and
#' equality-relevant state must remain stable while it belongs to a set.
#' Use `RSNull$null` to store a null member; R `NULL` is rejected. Set operations produce new immutable
#' sets without changing either operand.
#'
#' `copy()` returns the receiver. `mutableCopy()` creates independent native
#' storage that initially references the same member objects.
#'
#' @section Convenience Constructor:
#' [RSSet_withSet()] creates an immutable shallow copy from a mutable set with
#' independent native storage. An immutable set is returned as is.
#'
#' @export
RSSet <- RSObjectClass(
  "RSSet",
  version = "0.1.1 beta",
  super_class = RSObject,
  protocols = list(RSCopying, RSMutableCopying, RSCoding),
  private = list(
    .storage = NULL,

    .storeObject = function(item) {
      item <- .robjc_collection_object(item)
      hash <- item$hash()
      if (!is.integer(hash) || length(hash) != 1L || is.na(hash))
        stop("Member hash() must return one non-missing integer.", call. = FALSE)
      private$.storage$add(item)
      invisible(self)
    }
  ),
  public = list(
    #' @description Constructs an immutable set retaining the supplied objects.
    #' @param objects An optional list or RObjC collection of RObjC objects,
    #'   class generators, or protocol generators. Use `RSNull$null` for a null member.
    #' @return A new `RSSet` instance.
    initialize = function(objects = list()) {
      objects <- cast_collections_to_list(objects)
      valid <- vapply(
        objects,
        function(object) robjc_isInstance(object) ||
          robjc_isMetaClass(object),
        logical(1)
      )
      if (!all(valid)) stop("All elements must be RObjC objects, classes, or protocols; use RSNull$null for null members.")

      super$initialize()
      private$.storage <- RSConcreteSet$new()
      for (item in objects) private$.storeObject(item)
      invisible(self)
    },

    #' @description Returns the number of members.
    #' @return A non-negative numeric scalar.
    count = function() private$.storage$count(),

    #' @description Returns the stored member equal to `object`.
    #' @param object An RObjC object, class generator, protocol generator, or
    #'   [RSNull] to look up a null member. R `NULL` is rejected.
    #' @return The stored member, or `NULL` when absent.
    member = function(object) {
      private$.storage$member(.robjc_collection_object(object))
    },

    #' @description Tests whether an equal member is present.
    #' @param object An RObjC object, class generator, protocol generator, or
    #'   [RSNull] to look up a null member. R `NULL` is rejected.
    #' @return A logical scalar.
    containsObject = function(object) {
      private$.storage$contains(.robjc_collection_object(object))
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

    #' @description Computes an order-independent aggregate member hash.
    #' The final result passes through [robjc_normalize_hash()].
    #' @return One non-missing integer hash value.
    hash = function() robjc_normalize_hash(private$.storage$contentHash()),

    #' @description Compares two sets by membership.
    #' @param other Another set.
    #' @return `TRUE` when both sets contain equal members.
    isEqual = function(other) {
      if (!robjc_isInstance(other) ||
          !other$isKindOf(RSSet) ||
          self$count() != other$count()) {
        return(FALSE)
      }
      all(vapply(
        self$allObjectsAsList(),
        function(item) isTRUE(other$containsObject(item)),
        logical(1)
      ))
    },

    #' @description Returns RObjC instance members for legacy serialization
    #'   hooks. Keyed archives use `encodeWithCoder()` for all members.
    #' @return A list of RObjC instance members.
    objectsForEncoding = function() Filter(robjc_isInstance, self$allObjectsAsList()),

    #' @description Encodes all members. Their order has no meaning, while
    #'   the coder preserves shared references to individual members.
    #' @param coder An [RSKeyedArchiver] recording keyed values.
    #' @return `self`, invisibly.
    encodeWithCoder = function(coder) {
      coder$encodeObjectForKey(self$allObjectsAsList(), "objects")
      invisible(self)
    },

    #' @description Restores native storage without calling `initialize()`.
    #'   This serves both immutable and mutable sets.
    #' @param coder An [RSKeyedUnarchiver] reading keyed values.
    #' @return `self`, invisibly.
    initWithCoder = function(coder) {
      objects <- coder$decodeObjectForKey("objects")
      if (
        !is.list(objects) ||
        !all(vapply(objects,
          function(item) {robjc_isInstance(item) || robjc_isMetaClass(item)},
          logical(1))
        )
      )
        stop("Decoded set members must be RObjC objects, classes, or protocols.",
             call. = FALSE)
      storage <- RSConcreteSet$new()
      for (item in objects) {
        hash <- item$hash()
        if (!is.integer(hash) || length(hash) != 1L || is.na(hash))
          stop("Member hash() must return one non-missing integer.", call. = FALSE)
        storage$add(item)
      }
      private$.storage <- storage
      invisible(self)
    },

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

    #' @description Returns the receiver because it is immutable.
    #' @return `self`.
    copy = function() self,

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
      robjc_isInstance(set) && set$isKindOf(RSSet)
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
