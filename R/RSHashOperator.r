#' @include RSHashOperatorProtocol.r
#' @include RSModule.r
#' @title RSHashOperator: Shared Hash and Set Operations
#'
#' @description
#' `RSHashOperator` provides scalar hashing and operations on lists of hashable
#' RObjC objects or Class/Protocol generators. `RSSingletonClass()` creates the shared instance eagerly;
#' access it through `RSHashOperator$instance`.
#'
#' @return The `RSHashOperator` class generator.
#'
#' @export
RSHashOperator <- RSSingletonClass(
  "RSHashOperator",
  version = "0.0.3 alpha",
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
          length(val) != 1L || !is.null(dim(val)))
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
      if (!is.character(string) || length(string) != 1L || is.na(string)) {
        stop("string must be one non-missing character value", call. = FALSE)
      }
      chars <- utf8ToInt(enc2utf8(string))
      hash <- 0
      for (c in chars) {
        hash <- (hash * 31 + c) %% 2147483647
      }
      as.integer(hash)
    },


    #' @description
    #' Computes a hash for each object in a list using its `hash()` method.
    #' Rejects a missing hash returned by any object.
    #' @param objects A list of hashable RObjC objects or Class/Protocol generators.
    #' @return An integer vector of non-missing hash codes.
    hashesOfEachObject = function(objects) {
      if (length(objects) == 0L) return(integer())
      hashes <- vapply(objects, function(obj) obj$hash(), integer(1))
      if (anyNA(hashes))
        stop("Object hashes must be non-missing integers.", call. = FALSE)
      hashes
    },



    #' @description
    #' Computes a single aggregate hash value by XOR-ing the hashes of a list of objects.
    #' A missing component hash is rejected before aggregation.
    #' If the XOR result is R's `NA_integer_` bit pattern, returns `0L`.
    #' @param objects A list of hashable RObjC objects or Class/Protocol generators.
    #' @return One non-missing aggregate integer hash code.
    hashFromObjects = function(objects) {
      hashes <- self$hashesOfEachObject(objects)
      hash_code <- Reduce(bitwXor, hashes, init = 0L)
      if (is.na(hash_code)) 0L else hash_code
    },



    #' @description
    #' Determines whether two object lists represent the same set.
    #' Uses both hash comparison and `isEqual()` for deep equivalence. Missing
    #' object hashes raise an error. List names do not affect equality.
    #' @param obj1 A list of hashable RObjC objects or Class/Protocol generators.
    #' @param obj2 Another such list to compare against.
    #' @return `TRUE` if the sets are equal in content and order-independent; otherwise `FALSE`.
    areEqualSets = function(obj1, obj2) {
      if (length(obj1) != length(obj2)) return(FALSE)
      obj1_hashes <- self$hashesOfEachObject(obj1)
      obj2_hashes <- self$hashesOfEachObject(obj2)
      if (!identical(sort(unname(obj1_hashes)), sort(unname(obj2_hashes))))
        return(FALSE)
      differs_by_method <- any(vapply(
        seq_along(obj1),
        function(i) {
          !any(vapply(seq_along(obj2), function(j) {
            obj1_hashes[[i]] == obj2_hashes[[j]] &&
              isTRUE(obj1[[i]]$isEqual(obj2[[j]]))
          }, logical(1)))
        },
        logical(1)
      ))
      return(!differs_by_method)
    },



    #' @description
    #' Returns the subset of objects in `obj1` that are also present in `obj2`.
    #' Matching is determined via both hash equality and `isEqual()`. Missing
    #' object hashes raise an error.
    #' @param obj1 A list of hashable RObjC objects or Class/Protocol generators to filter.
    #' @param obj2 Another such list to match against.
    #' @return A list of objects from `obj1` that are also found in `obj2`.
    objectsAlsoIn = function(obj1, obj2) {
      obj1_hashes <- self$hashesOfEachObject(obj1)
      obj2_hashes <- self$hashesOfEachObject(obj2)
      keep <- vapply(
        seq_along(obj1),
        function(i) {
          if (!(obj1_hashes[[i]] %in% obj2_hashes)) return(FALSE)
          any(vapply(
            seq_along(obj2),
            function(j) {
              isTRUE(obj1_hashes[[i]] == obj2_hashes[[j]] &&
                       obj1[[i]]$isEqual(obj2[[j]]))
            },
            logical(1)
          ))
        },
        logical(1)
      )
      obj1[keep]
    },



    #' @description
    #' Returns the subset of objects in `obj1` that are not present in `obj2`.
    #' Uses hash filtering for early rejection and `isEqual()` for final match
    #' checking. Missing object hashes raise an error.
    #' @param obj1 A list of hashable RObjC objects or Class/Protocol generators to filter.
    #' @param obj2 Another such list used as the exclusion base.
    #' @return A list of objects in `obj1` not found in `obj2`.
    objectsNotIn = function(obj1, obj2) {
      obj1_hashes <- self$hashesOfEachObject(obj1)
      obj2_hashes <- self$hashesOfEachObject(obj2)
      keep <- vapply(
        seq_along(obj1),
        function(i) {
          # Step 1: early reject via hash mismatch
          if (!(obj1_hashes[[i]] %in% obj2_hashes)) return(TRUE)
          # Step 2: confirm absence by strict isEqual check
          !any(vapply(
            obj2, function(o2) isTRUE(obj1[[i]]$isEqual(o2)), logical(1)
          ))
        },
        logical(1)
      )
      obj1[keep]
    },



    #' @description
    #' Returns the union of two sets, combining all unique elements from `obj1` and `obj2`.
    #' This is a non-destructive operation: the result includes all elements of `obj2`,
    #' plus elements of `obj1` not found in `obj2`.
    #' @param obj1 A list of hashable RObjC objects or Class/Protocol generators.
    #' @param obj2 Another such list.
    #' @return A list representing the union of both sets.
    unionOfSets = function(obj1, obj2) {
      c(obj2, self$objectsNotIn(obj1, obj2))
    },



    #' @description
    #' Returns the intersection of two sets — i.e., elements common to both `obj1` and `obj2`.
    #' Matching is based on both hash and `isEqual()` checks.
    #' @param obj1 A list of hashable RObjC objects or Class/Protocol generators.
    #' @param obj2 Another such list.
    #' @return A list of objects present in both `obj1` and `obj2`.
    intersectionOfSets = function(obj1, obj2) {
      self$objectsAlsoIn(obj1, obj2)
    },



    #' @description
    #' Returns the difference of two sets — i.e., elements in `obj1` that are not in `obj2`.
    #' Functionally equivalent to `objectsNotIn()`.
    #' @param obj1 A list of hashable RObjC objects or Class/Protocol generators.
    #' @param obj2 Another such list to exclude.
    #' @return A list of objects in `obj1` but not in `obj2`.
    differenceOfSets = function(obj1, obj2) {
      self$objectsNotIn(obj1, obj2)
    }
  )
)
