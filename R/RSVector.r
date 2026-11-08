#' @include RSHashOperator.r RSMutableCopying.r RSCopying.r RSListing.r RSEncoding.r
#' @title RSVector: Immutable Atomic Vector Container
#' @description Wraps an atomic vector with indexed access, equality,
#' comparison, hashing, and coding. The stored values cannot be changed.
#' Use `mutableCopy()` to obtain an editable vector.
#' Hashes use the standard [RSHashOperator] instance.
#' @section Protocols:
#' - [RSMutableCopying]
#' - [RSCopying]
#' - [RSComparable]
#' - [RSListing]
#' - [RSCoding]
#'
#' @examples
#' values <- RSVector$new(c(10L, 20L, 30L))
#' values$valueAtIndex(2L)
#' values$indexesOfValues(c(30L, 99L))
#' editable <- values$mutableCopy()
#' editable$appendValues(40L)
#' values$values()  # The immutable receiver still has three values.
#'
#' @return The `RSVector` class generator.
#' @export
RSVector <- RSObjectClass(
  "RSVector",
  version = "0.2.0",
  super_class = RSObject,
  protocols = list(RSMutableCopying, RSCopying, RSComparable, RSListing, RSCoding),
  private     = list(
    .vec = NULL,
    .validateValues = function(values) {
      if (!is.atomic(values) ||
          !is.null(dim(values)) ||
          !identical(typeof(values), typeof(private$.vec)) ||
          !identical(class(values), class(private$.vec)) ||
          (is.factor(private$.vec) && !identical(levels(values), levels(private$.vec))))
        stop("`values` must have the vector's type and class.", call. = FALSE)
      invisible(TRUE)
    }
  ),
  public = list(
    #' @description Initialize the vector.
    #' @param vec An atomic vector without dimensions.
    #' @return The initialized instance, invisibly.
    initialize = function(vec) {
      super$initialize()
      if (!is.atomic(vec) || !is.null(dim(vec)))
        stop("`vec` must be an atomic vector.", call. = FALSE)
      private$.vec <- vec
      invisible(self)
    },



    #' @description Encodes the atomic vector as a base R serialization value.
    #' @param coder An [RSKeyedArchiver] recording keyed values.
    #' @return `self`, invisibly.
    encodeWithCoder = function(coder) {
      coder$encodeObjectForKey(private$.vec, "values")
      invisible(self)
    },



    #' @description Restores the vector without calling
    #'   `initialize()`. Also serves immutable [RSVector] subclasses.
    #' @param coder An [RSKeyedUnarchiver] reading keyed values.
    #' @return `self`, invisibly.
    initWithCoder = function(coder) {
      vec <- coder$decodeObjectForKey("values")
      if (!is.atomic(vec) || !is.null(dim(vec)))
        stop("Decoded vector must be atomic and have no dimensions.", call. = FALSE)
      private$.vec <- vec
      invisible(self)
    },



    #' @description Return the stored values.
    #' @return The atomic vector.
    values = function() private$.vec,



    #' @description Return the value at a one-based index.
    #' @param index Numeric position to read; converted with `as.integer()`.
    #' @return A length-one value.
    valueAtIndex = function(index) {
      invalid <- find_invalid_indexes(index, length(private$.vec), scalar = TRUE)
      if (length(invalid))
        stop("Index out of bounds: ", toString(invalid), ".", call. = FALSE)
      index <- as.integer(index)
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
    indexesOfValues = function(values) {
      private$.validateValues(values)
      match(values, private$.vec)
    },



    #' @description Return the stored atomic vector.
    #' @return The atomic vector.
    asVector = function() private$.vec,



    #' @description Returns individual native values as a list for iteration.
    #'   Value classes such as factor and Date are preserved by `as.list()`.
    #' @return An R list in vector order.
    asList = function() as.list(private$.vec),



    #' @description Return the number of values.
    #' @return An integer count.
    count = function() length(private$.vec),



    #' @description Return this immutable instance.
    #' @return `self`.
    copy = function() self,



    #' @description Create an independent mutable vector with the same values.
    #' @return A new `RSMutableVector` instance.
    mutableCopy = function() RSMutableVector$new(private$.vec),



    #' @description
    #' Computes an aggregate hash by combining the hash values of all elements.
    #' This is used for fast comparison and dictionary key support.
    #' The result passes through [robjc_normalize_hash()], mapping R's
    #' `NA_integer_` bit pattern to `0L`.
    #'
    #' @return One non-missing integer hash value.
    hash = function() {
      hashes <- self$hashesOfValues()
      hash_code <- Reduce(bitwXor, hashes, init = 0L)
      robjc_normalize_hash(hash_code)
    },



    #' @description
    #' Computes a hash code for each scalar value in the vector.
    #' A missing result from the hash operator raises an error.
    #'
    #' @return An integer vector of non-missing hashes.
    hashesOfValues = function() {
      if (length(private$.vec) == 0L) return(integer())
      hashes <- vapply(private$.vec, RSHashOperator$instance$hashForScalar, integer(1))
      if (anyNA(hashes))
        stop("`hashForScalar()` must return non-missing integers.", call. = FALSE)
      hashes
    },



    #' @description
    #' Returns a concise textual summary of the vector,
    #' including its class name and the current number of stored values.
    #' This is primarily used for human-readable identification and logging.
    #'
    #' @return A character string in the form \code{"<ClassName: N values>"},
    #' where \code{ClassName} is the actual class of the object and \code{N} is the number of elements it contains.
    description = function() sprintf("<%s: %d values>", self$class()$classname, self$count()),



    #' @description
    #' Compares this vector to another for equality, including value attributes.
    #'
    #' @param other Another vector object to compare with.
    #' @return `TRUE` if the vectors are equal; otherwise `FALSE`.
    isEqual = function(other) {
      if (!robjc_isInstance(other) || !other$isKindOf(RSVector))
        return(FALSE)
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
      if (!robjc_isInstance(other) || !other$isKindOf(RSVector))
        stop("Cannot compare objects of different class.")
      left  <- private$.vec
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
        left_na   <- is.na(left[i])
        right_na  <- is.na(right[i])
        if (left_na && right_na)  next
        if (left_na)              return(if (na.last) 1L  else -1L)
        if (right_na)             return(if (na.last) -1L else 1L)
        pair <- tryCatch(
          xtfrm(c(left[i], right[i])),
          error = function(e) stop("Vector values cannot be ordered.", call. = FALSE)
        )
        if (pair[1L] < pair[2L])  return(-1L)
        if (pair[1L] > pair[2L])  return(1L)
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
      if (!robjc_isInstance(other) || !other$isKindOf(RSVector))
        stop("`other` must be a vector object.", call. = FALSE)
      left <- private$.vec
      right <- other$asVector()
      for (i in seq_len(min(length(left), length(right)))) {
        left_value  <- left[i]
        right_value <- right[i]
        left_na     <- isTRUE(is.na(left_value))
        right_na    <- isTRUE(is.na(right_value))
        if (left_na && right_na)                next
        if (left_na)                            return(if (na.last) 1L  else -1L)
        if (right_na)                           return(if (na.last) -1L else 1L)
        if (isTRUE(left_value == right_value))  next
        less <- left_value < right_value
        if (isTRUE(less))                       return(-1L)
        if (length(less) != 1L || is.na(less))
          stop("Native values have no ordering.", call. = FALSE)
        greater <- left_value > right_value
        if (isTRUE(greater))                    return(1L)
        stop("Native values have no ordering.", call. = FALSE)
      }
      as.integer(sign(length(left) - length(right)))
    }
  )
)
