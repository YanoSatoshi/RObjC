#' @include RSMutableVector.r
#' @title RSVector: Immutable Atomic Vector Container
#'
#' @description
#' `RSVector` is the read-only counterpart of [RSMutableVector]. It inherits
#' indexed access, equality, comparison, and hashing while rejecting changes
#' to its contents. Use `mutableCopy()` to obtain an editable vector.
#'
#' @examples
#' vector <- RSVector$new(c(1L, 2L))
#' vector$valueAtIndex(2L)
#' editable <- vector$mutableCopy()
#' editable$appendValues(3L)
#'
#' @return The `RSVector` class generator.
#' @export
RSVector <- RSObjectClass(
  "RSVector",
  version = "0.0.4 alpha",
  super_class = RSMutableVector,
  protocols = list(RSCopying, RSMutableCopying),
  public = list(
    #' @description Return this immutable instance.
    #' @return `self`.
    copy = function() self,

    #' @description Create an independent mutable vector with the same values.
    #' @return A new `RSMutableVector` instance.
    mutableCopy = function() {
      RSMutableVector$new(
        private$.vec,
        hash_operator = private$.hash_operator
      )
    },

    #' @description Disabled for immutable vectors.
    #' @param ... Ignored; this method always raises an error.
    appendValues = function(...) {
      stop(msg_immutable_try_mutate(self$class()), call. = FALSE)
    },

    #' @description Disabled for immutable vectors.
    #' @param ... Ignored; this method always raises an error.
    insertValuesAtIndex = function(...) {
      stop(msg_immutable_try_mutate(self$class()), call. = FALSE)
    },

    #' @description Disabled for immutable vectors.
    #' @param ... Ignored; this method always raises an error.
    replaceValuesAtIndices = function(...) {
      stop(msg_immutable_try_mutate(self$class()), call. = FALSE)
    },

    #' @description Disabled for immutable vectors.
    #' @param ... Ignored; this method always raises an error.
    removeValuesAtIndices = function(...) {
      stop(msg_immutable_try_mutate(self$class()), call. = FALSE)
    },

    #' @description Disabled for immutable vectors.
    #' @param ... Ignored; this method always raises an error.
    removeAllValues = function(...) {
      stop(msg_immutable_try_mutate(self$class()), call. = FALSE)
    }
  )
)
