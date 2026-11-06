#' @include RSVector.r
#' @title RSMutableVector: Mutable Atomic Vector Container
#' @description Extends [RSVector] with methods that change the stored
#' atomic vector. Values retain the vector's type and class; numeric indices
#' are converted with `as.integer()` before bounds checks.
#' @section Protocols:
#' - [RSMutableCopying]
#' - [RSCopying]
#' - [RSComparable]
#' - [RSListing]
#' - [RSCoding]
#'
#' @return The `RSMutableVector` class generator.
#' @export
RSMutableVector <- RSObjectClass(
  "RSMutableVector",
  version = "0.1.0 beta",
  super_class = RSVector,
  protocols = list(RSMutableCopying, RSCopying, RSComparable, RSListing, RSCoding),
  public = list(
    #' @description Append values to the vector.
    #' @param values Values with the stored vector's type and class.
    #' @return The instance, invisibly.
    appendValues = function(values) {
      private$.validateValues(values)
      result <- append(private$.vec, values)
      private$.validateValues(result)
      private$.vec <- result
      invisible(self)
    },



    #' @description Insert values before a one-based index.
    #' @param values Values with the stored vector's type and class.
    #' @param index Numeric position from 1 through `count() + 1`; converted
    #'   with `as.integer()`.
    #' @return The instance, invisibly.
    insertValuesAtIndex = function(values, index) {
      private$.validateValues(values)
      invalid <- find_invalid_indexes(index, length(private$.vec),
                                      allow_end = TRUE, scalar = TRUE)
      if (length(invalid))
        stop("Index out of bounds: ", toString(invalid), ".", call. = FALSE)
      index <- as.integer(index)
      result <- append(private$.vec, values, after = index - 1L)
      private$.validateValues(result)
      private$.vec <- result
      invisible(self)
    },



    #' @description Replace values at the specified one-based indices.
    #' @param values Replacement values, one per index, with the stored vector's
    #'   type and class.
    #' @param indices Numeric positions to replace; converted with `as.integer()`.
    #' @return The instance, invisibly.
    replaceValuesAtIndexes = function(values, indices) {
      private$.validateValues(values)
      invalid <- find_invalid_indexes(indices, length(private$.vec))
      if (length(invalid))
        stop("Index out of bounds: ", toString(invalid), ".", call. = FALSE)
      indices <- as.integer(indices)
      if (length(values) != length(indices))
        stop("`values` and `indices` must have the same length.", call. = FALSE)
      result <- private$.vec
      result[indices] <- values
      private$.validateValues(result)
      private$.vec <- result
      invisible(self)
    },



    #' @description Remove values at the specified one-based indices.
    #' @param indices Numeric positions to remove; converted with `as.integer()`.
    #' @return The instance, invisibly.
    removeValuesAtIndexes = function(indices) {
      invalid <- find_invalid_indexes(indices, length(private$.vec))
      if (length(invalid))
        stop("Index out of bounds: ", toString(invalid), ".", call. = FALSE)
      indices <- as.integer(indices)
      if (length(indices) == 0L) return(invisible(self))
      result <- private$.vec[-unique(indices)]
      private$.validateValues(result)
      private$.vec <- result
      invisible(self)
    },



    #' @description Remove every value while preserving the vector type.
    #' @return The instance, invisibly.
    removeAllValues = function() {
      result <- private$.vec[-seq_along(private$.vec)]
      private$.validateValues(result)
      private$.vec <- result
      invisible(self)
    },



    #' @description Create an immutable `RSVector` with the same values.
    #' @return A new `RSVector` instance.
    copy = function() {
      RSVector$new(private$.vec)
    }
  )
)
