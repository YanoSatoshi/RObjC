#' @include RSHashOperator.r RSMutableCopying.r RSCopying.r
#' @title RSMutableVector: Mutable Atomic Vector Container
#' @description Wraps an atomic vector with indexed mutation and copying methods.
#' Added and searched values must have the stored vector's type and class.
#' Numeric indices are converted with `as.integer()` before bounds checks.
#' @return The `RSMutableVector` class generator.
#' @export
RSMutableVector <- RSObjectClass(
  "RSMutableVector",
  version     = "0.0.3 alpha",
  super_class = RSObject,
  protocols   = list(RSMutableCopying, RSCopying),
  public      = list(
    #' @description Initialize the vector and its hash operator.
    #' @param vec An atomic vector without dimensions.
    #' @param hash_operator An object providing `hashForScalar()`.
    #' @return The initialized instance, invisibly.
    initialize = function(
      vec,
      hash_operator = RSHashOperator$instance
    ) {
      super$initialize()
      if (!is.atomic(vec) || !is.null(dim(vec)))
        stop("`vec` must be an atomic vector.", call. = FALSE)
      if (!robjc_isRobjcObject(hash_operator) ||
          !hash_operator$respondsTo("hashForScalar"))
        stop("`hash_operator` must provide `hashForScalar()`.", call. = FALSE)
      private$.vec <- vec
      private$.hash_operator <- hash_operator
      invisible(self)
    },
    

    #' @description Return the stored values.
    #' @return The atomic vector.
    values = function() private$.vec,


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
      index <- private$.validatedIndices(index, allow_end = TRUE, scalar = TRUE)
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
    replaceValuesAtIndices = function(values, indices) {
      private$.validateValues(values)
      indices <- private$.validatedIndices(indices)
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
    removeValuesAtIndices = function(indices) {
      indices <- private$.validatedIndices(indices)
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



    #' @description Return the value at a one-based index.
    #' @param index Numeric position to read; converted with `as.integer()`.
    #' @return A length-one value.
    valueAtIndex = function(index) {
      index <- private$.validatedIndices(index, scalar = TRUE)
      private$.vec[index]
    },



    #' @description Test whether each value occurs in the vector.
    #' @param values Values with the stored vector's type and class to search for.
    #' @return A logical vector.
    containsValues = function(values) {
      private$.validateValues(values)
      values %in% private$.vec
    },



    #' @description Find the first matching index for each value.
    #' @param values Values with the stored vector's type and class to search for.
    #' @return Integer indices, with `NA` for values not found.
    indicesOfValues = function(values) {
      private$.validateValues(values)
      match(values, private$.vec)
    },



    #' @description Return the stored atomic vector.
    #' @return The atomic vector.
    asVector = function() private$.vec,



    #' @description Return the number of values.
    #' @return An integer count.
    count = function() length(private$.vec),



    #' @description Create an immutable `RSVector` with the same values.
    #' @return A new `RSVector` instance.
    copy = function() {
      RSVector$new(
        private$.vec,
        hash_operator = private$.hash_operator
      )
    },



    #' @description
    #' Returns a new mutable vector with the same values and hash operator.
    #' 
    #' @return A new `RSMutableVector` instance.
    mutableCopy = function() {
      RSMutableVector$new(
        private$.vec,
        hash_operator = private$.hash_operator
      )
    },



    #' @description
    #' Computes an aggregate hash by combining the hash values of all elements.
    #' This is used for fast comparison and dictionary key support.
    #'
    #' @return An integer hash value.
    hash = function() {
      hashes <- self$hashesOfValues()
      Reduce(bitwXor, hashes, init = 0L)
    },



    #' @description
    #' Computes a hash code for each scalar value in the vector.
    #'
    #' @return An integer vector of hashes.
    hashesOfValues = function() {
      if (length(private$.vec) == 0) return(0L)
      operator <- self$hashOperator()
      vapply(private$.vec, operator$hashForScalar, integer(1))
    },



    #' @description
    #' Returns the `RSHashOperator` module instance used for hashing.
    #' This method provides centralized access to hashing behavior.
    #'
    #' @return The singleton instance of `RSHashOperator`.
    hashOperator = function() private$.hash_operator,



    #' @description
    #' Returns a concise textual summary of the vector,
    #' including its class name and the current number of stored values.
    #' This is primarily used for human-readable identification and logging.
    #'
    #' @return A character string in the form \code{"<ClassName: N values>"},
    #' where \code{ClassName} is the actual class of the object and \code{N} is the number of elements it contains.
    description = function() {
      sprintf("<%s: %d values>", self$class()$classname, self$count())
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
    #' Compares this vector to another for equality, including value attributes.
    #'
    #' @param other Another vector object to compare with.
    #' @return `TRUE` if the vectors are equal; otherwise `FALSE`.
    isEqual = function(other) {
      if (!robjc_isRobjcObject(other) || !other$isMemberOf(self$class()))
        return(FALSE)
      identical(private$.vec, other$asVector())
    },



    #' @description
    #' Compares this vector to another for ordering.
    #' The comparison is lexicographic: each element is compared in order using `compare()`.
    #' If all elements are equal, the shorter array is considered smaller.
    #'
    #' @param other Another `RSMutableVector` instance.
    #' @param na.last Logical; if `TRUE`, NA values are treated as greater.
    #' @return -1 if less than, 0 if equal, 1 if greater.
    compare = function(other, na.last = TRUE) {
      if (!is.logical(na.last) || length(na.last) != 1L || is.na(na.last))
        stop("`na.last` must be TRUE or FALSE.")
      if (!robjc_isRobjcObject(other) || !other$isMemberOf(self$class()))
        stop("Cannot compare objects of different class.")
      left <- private$.vec
      right <- other$asVector()
      for (i in seq_len(min(length(left), length(right)))) {
        left_na <- is.na(left[i])
        right_na <- is.na(right[i])
        if (left_na && right_na) next
        if (left_na) return(if (na.last) 1L else -1L)
        if (right_na) return(if (na.last) -1L else 1L)
        pair <- tryCatch(
          xtfrm(c(left[i], right[i])),
          error = function(e) stop("Vector values cannot be ordered.", call. = FALSE)
        )
        if (pair[1L] < pair[2L]) return(-1L)
        if (pair[1L] > pair[2L]) return(1L)
      }
      as.integer(sign(length(left) - length(right)))
    }
  ),

  
  private     = list(
    .vec = NULL,
    .hash_operator = NULL,
    .validateValues = function(values) {
      if (!is.atomic(values) || !is.null(dim(values)) ||
          !identical(typeof(values), typeof(private$.vec)) ||
          !identical(class(values), class(private$.vec)) ||
          (is.factor(private$.vec) &&
           !identical(levels(values), levels(private$.vec)))) {
        stop("`values` must have the vector's type and class.", call. = FALSE)
      }
      invisible(TRUE)
    },
    .validatedIndices = function(indices, allow_end = FALSE, scalar = FALSE) {
      if (!is.numeric(indices) || (scalar && length(indices) != 1L) ||
          anyNA(indices) || any(!is.finite(indices))) {
        stop("Index out of bounds.", call. = FALSE)
      }
      indices <- suppressWarnings(as.integer(indices))
      if (anyNA(indices) ||
          any(indices < 1L | indices > length(private$.vec) + as.integer(allow_end))) {
        stop("Index out of bounds.", call. = FALSE)
      }
      indices
    }
  )
)
