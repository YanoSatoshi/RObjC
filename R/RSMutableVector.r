#' @include RSHashOperator.r RSMutableCopying.r RSCopying.r
#' @title RSMutableVector: Mutable Atomic Vector Container
#' @description Wraps an atomic vector with indexed mutation and copying methods.
#' Added and searched values must have the stored vector's type and class.
#' Numeric indices are converted with `as.integer()` before bounds checks.
#' A supplied hash operator must hash equal stored values consistently and
#' remain stable while the vector is used as a key or set member.
#' @return The `RSMutableVector` class generator.
#' @export
RSMutableVector <- RSObjectClass(
  "RSMutableVector",
  version     = "0.0.3 alpha",
  super_class = RSObject,
  protocols   = list(RSMutableCopying, RSCopying, RSComparable),
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
    #' If the XOR result is R's `NA_integer_` bit pattern, returns `0L`.
    #'
    #' @return One non-missing integer hash value.
    hash = function() {
      hashes <- self$hashesOfValues()
      hash_code <- Reduce(bitwXor, hashes, init = 0L)
      if (is.na(hash_code)) 0L else hash_code
    },



    #' @description
    #' Computes a hash code for each scalar value in the vector.
    #' A missing result from the hash operator raises an error.
    #'
    #' @return An integer vector of non-missing hashes.
    hashesOfValues = function() {
      if (length(private$.vec) == 0L) return(integer())
      operator <- self$hashOperator()
      hashes <- vapply(private$.vec, operator$hashForScalar, integer(1))
      if (anyNA(hashes))
        stop("`hashForScalar()` must return non-missing integers.", call. = FALSE)
      hashes
    },



    #' @description
    #' Returns the hash operator supplied at initialization, or the default
    #' [RSHashOperator] instance when none was supplied.
    #'
    #' @return The stored hash operator. The default is `RSHashOperator$instance`.
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
    #' Compares this vector to another for equality, including value attributes
    #' and the identity of the hash operator.
    #'
    #' @param other Another vector object to compare with.
    #' @return `TRUE` if the vectors are equal; otherwise `FALSE`.
    isEqual = function(other) {
      if (!robjc_isRobjcObject(other) || !other$isKindOf(RSMutableVector))
        return(FALSE)
      identical(private$.hash_operator, other$hashOperator()) &&
        identical(private$.vec, other$asVector())
    },



    #' @description
    #' Compares this vector to another for ordering.
    #' The comparison is lexicographic: each element is compared in order using
    #' base R's sorting order. Stored vectors must have the same `typeof()` and
    #' `class()`; use `nativeCompare()` for base R's cross-type value comparison.
    #' If all elements are equal, the shorter vector is considered smaller.
    #' Factor vectors with different levels cannot be ordered against each other.
    #' Factor and non-factor vectors have no shared sorting order.
    #' Raw vectors cannot be ordered, including empty raw vectors.
    #'
    #' @param other Another `RSMutableVector` or `RSVector` instance.
    #' @param na.last Whether NA values are treated as greater. Coerced with
    #'   `as.logical()`; the result must be one nonmissing value.
    #' @return `-1L` if less than, `0L` if equal, or `1L` if greater.
    compare = function(other, na.last = TRUE) {
      na.last <- tryCatch(as.logical(na.last), error = function(e) NA)
      if (!is.logical(na.last) || length(na.last) != 1L || is.na(na.last))
        stop("`na.last` must be TRUE or FALSE.")
      if (!robjc_isRobjcObject(other) || !other$isKindOf(RSMutableVector))
        stop("Cannot compare objects of different class.")
      left <- private$.vec
      right <- other$asVector()
      if (xor(is.factor(left), is.factor(right)))
        stop("Factor and non-factor vectors have no shared ordering.", call. = FALSE)
      if (is.factor(left) && is.factor(right) && !identical(levels(left), levels(right)))
        stop("Factor level sets differ.", call. = FALSE)
      if (!identical(typeof(left), typeof(right)) || !identical(class(left), class(right)))
        stop("Vector value types are not comparable.", call. = FALSE)
      if (typeof(left) == "raw")
        stop("Raw vectors cannot be sorted by base R.", call. = FALSE)
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
    },



    #' @description Compare vectors lexicographically using base R's atomic
    #'   value operators (`==`, `<`, and `>`), rather than `compare()`'s sorting
    #'   rules. Each pair is compared as native R scalar values. Missing values
    #'   (including `NaN`) tie and are placed according to `na.last`.
    #' @param other Another `RSMutableVector` or `RSVector` instance.
    #' @param na.last Whether missing values sort after nonmissing values.
    #'   Coerced with `as.logical()`; the result must be one nonmissing value.
    #' @return `-1L`, `0L`, or `1L`. An error is raised when base R has no
    #'   ordering for unequal values.
    nativeCompare = function(other, na.last = TRUE) {
      na.last <- tryCatch(as.logical(na.last), error = function(e) NA)
      if (!is.logical(na.last) || length(na.last) != 1L || is.na(na.last))
        stop("`na.last` must be TRUE or FALSE.", call. = FALSE)
      if (!robjc_isRobjcObject(other) || !other$isKindOf(RSMutableVector))
        stop("`other` must be a vector object.", call. = FALSE)
      left <- private$.vec
      right <- other$asVector()
      for (i in seq_len(min(length(left), length(right)))) {
        left_value <- left[i]
        right_value <- right[i]
        left_na <- isTRUE(is.na(left_value))
        right_na <- isTRUE(is.na(right_value))
        if (left_na && right_na) next
        if (left_na) return(if (na.last) 1L else -1L)
        if (right_na) return(if (na.last) -1L else 1L)
        if (isTRUE(left_value == right_value)) next
        less <- left_value < right_value
        if (isTRUE(less)) return(-1L)
        if (length(less) != 1L || is.na(less))
          stop("Native values have no ordering.", call. = FALSE)
        greater <- left_value > right_value
        if (isTRUE(greater)) return(1L)
        stop("Native values have no ordering.", call. = FALSE)
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
