#' @include RSHashOperatorProtocol.r
#' @include RSModule.r
#' @title RSHashOperator: Shared Hash Computation
#'
#' @description
#' `RSHashOperator` hashes scalar values and aggregates hashes of lists of
#' RObjC objects or Class/Protocol generators. `RSSingletonClass()` creates the shared instance eagerly;
#' access it through `RSHashOperator$instance`.
#'
#' @section Protocols:
#' - [RSHashOperatorProtocol]
#' - [RSCopying]
#'
#' @return The `RSHashOperator` class generator.
#'
#' @export
RSHashOperator <- RSSingletonClass(
  "RSHashOperator",
  version = "0.1.0 beta",
  super_class = RSObject,
  protocols   = list(RSHashOperatorProtocol),
  public = list(
    #' @description
    #' Initializes an operator instance.
    #' @return The initialized instance, invisibly.
    initialize = function() {
      super$initialize()
      invisible(self)
    },



    #' @description
    #' Computes a hash for a scalar value after normalizing it to UTF-8 text.
    #' Integer and double representations of the same value intentionally produce
    #' the same hash. Missing values use a stable type-specific marker.
    #' @param val A length-one atomic value or `POSIXlt` value without dimensions.
    #' @return An integer hash value.
    hashForScalar = function(val) {
      if ((!is.atomic(val) && !inherits(val, "POSIXlt")) ||
          length(val) != 1L ||
          !is.null(dim(val)))
        stop("`val` must be a scalar.", call. = FALSE)
      if (is.na(val))
        string <- sprintf("<NA:%s>", typeof(val))
      else
        string <- enc2utf8(as.character(val))
      self$computeHashCodeWithString(string)
    },



    #' @description
    #' Computes a non-negative 31-bit hash code from a UTF-8 string.
    #' Intermediate calculations use doubles to avoid R integer overflow.
    #' @param string A character string.
    #' @return An integer hash value.
    computeHashCodeWithString = function(string) {
      if (!is.character(string) || length(string) != 1L || is.na(string))
        stop("string must be one non-missing character value", call. = FALSE)
      chars <- utf8ToInt(enc2utf8(string))
      hash <- 0
      for (c in chars)
        hash <- (hash * 31 + c) %% 2147483647
      as.integer(hash)
    },



    #' @description
    #' Computes a hash for each object in a list using its `hash()` method.
    #' Rejects a missing hash returned by any object.
    #' @param objects A list of hashable RObjC objects or Class/Protocol generators.
    #' @return An integer vector of non-missing hash codes.
    hashesOfObjects = function(objects) {
      if (length(objects) == 0L) return(integer())
      hashes <- vapply(objects, function(obj) obj$hash(), integer(1))
      if (anyNA(hashes))
        stop("Object hashes must be non-missing integers.", call. = FALSE)
      hashes
    },



    #' @description
    #' Computes a single aggregate hash value by XOR-ing the hashes of a list of objects.
    #' A missing component hash is rejected before aggregation.
    #' The result passes through [robjc_normalize_hash()], mapping R's
    #' `NA_integer_` bit pattern to `0L`.
    #' @param objects A list of hashable RObjC objects or Class/Protocol generators.
    #' @return One non-missing aggregate integer hash code.
    combinedHashFromObjects = function(objects) {
      hashes <- self$hashesOfObjects(objects)
      hash_code <- Reduce(bitwXor, hashes, init = 0L)
      robjc_normalize_hash(hash_code)
    }
  )
)
