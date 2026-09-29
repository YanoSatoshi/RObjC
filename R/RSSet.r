#' @title RSSet: Native-Backed Immutable Set
#'
#' @description
#' `RSSet` is the immutable counterpart of [RSMutableSet]. It uses the same
#' native-backed storage and membership semantics, while rejecting every public
#' mutation operation.
#'
#' Inserted objects are retained rather than copied. `copy()` therefore returns
#' the receiver, while `mutableCopy()` creates independent native storage that
#' initially references the same member objects.
#'
#' @section Convenience Constructor:
#' `RSSet$setWithSet(set)` creates an immutable shallow copy with independent
#' native storage.
#'
#' @export
RSSet <- R6::R6Class(
  "RSSet",
  inherit = RSMutableSet,
  cloneable = FALSE,
  public = list(
    #' @description Constructs an immutable set retaining the supplied objects.
    #' @param objects An optional list or RObjC collection of members. `NULL`
    #'   elements are represented by [RSNull].
    #' @return A new `RSSet` instance.
    initialize = function(objects = list()) {
      super$initialize(objects = objects)
      invisible(self)
    },

    #' @description Disabled because the set is immutable.
    #' @param object Ignored.
    addObject = function(object) {
      stop(msg_immutable_try_mutate(self$class()))
    },

    #' @description Disabled because the set is immutable.
    #' @param object Ignored.
    removeObject = function(object) {
      stop(msg_immutable_try_mutate(self$class()))
    },

    #' @description Disabled because the set is immutable.
    removeAllObjects = function() {
      stop(msg_immutable_try_mutate(self$class()))
    },

    #' @description Returns the receiver because it is immutable.
    #' @return `self`.
    copy = function() self,

    #' @description Returns an independent mutable shallow copy.
    #' @return A new [RSMutableSet].
    mutableCopy = function() RSMutableSet$setWithSet(self)
  )
)

# Creates an immutable set with independent native storage.
RSSet$setWithSet <- function(set) {
  .rs_set_with_storage(
    RSSet,
    .rs_set_storage(set)$duplicate()
  )
}
