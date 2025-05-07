#' @title Register a Singleton Instance of an RSModule Subclass
#'
#' @description
#' Registers a singleton module object on the given class and defines its `$module()` accessor.
#' This function also locks the `.singleton` slot and the module object itself using `lockVariables()`.
#' It is invoked internally when registering an instance of an `RSModule` subclass.
#'
#' @param klass The R6 class object (subclass of `RSModule`)
#' @param obj The instance of the module to register
#'
#' @return The registered singleton object (invisibly).
#'
#' @examples
#' register_rs_module(ConfigModule, ConfigModule$new())
#'
#' @export
RSHashOperator <- R6::R6Class(
  "RSHashOperator",
  inherit = RSModule,
  cloneable = FALSE,
  public = list(
    initialize = function() {
      super$initialize()
      invisible(self)
    },


#' @describeIn RSHashOperator
#' Computes a hash for a scalar value by converting it to UTF-8 and applying a position-based bitwise XOR.
#' @param val A scalar value (e.g., number, string).
#' @return An integer hash value.
    hashForScalar = function(val) {
      str_x <- as.character(val)
      code_points <- utf8ToInt(str_x)
      if (length(code_points) == 0) return(0L)
      shifts  <- bitwShiftR((seq_along(code_points) - 1L), 5L)
      shifted <- bitwShiftL(code_points, shifts)
      h <- Reduce(bitwXor, shifted, init = 0L)
      abs(as.integer(h))
    },


#' @describeIn RSHashOperator
#' Computes a 32-bit signed integer hash code from a UTF-8 string using a simple hash function.
#' @param string A character string.
#' @return An integer hash value.
    computeHashCodeWithString = function(string) {
      chars <- utf8ToInt(string)
      hash <- 0L
      for (c in chars) {
        hash <- bitwAnd(bitwShiftL(hash, 5L) - hash + c, 0x7FFFFFFF)
      }
      as.integer(bitwAnd(hash, 0x7FFFFFFF))  # cut to signed 32bit
    },


#' @describeIn RSHashOperator
#' Computes a hash for each object in a list using its `hash()` method.
#' @param objects A list of RSScalar-compatible objects.
#' @return An integer vector of hash codes.
    hashesOfEachObject = function(objects) {
      if (length(objects) == 0) return(0L)
      vapply(objects, function(obj) obj$hash(), integer(1))
    },



#' @describeIn RSHashOperator
#' Computes a single aggregate hash value by XOR-ing the hashes of a list of objects.
#' @param objects A list of hashable RS objects.
#' @return A combined integer
    hashFromObjects = function(objects) {
      hashes <- vapply(objects, function(obj) obj$hash(), integer(1))
      Reduce(bitwXor, hashes)
    },



#' @describeIn RSHashOperator
#' Determines whether two object lists represent the same set.
#' Uses both hash comparison and `isEqual()` for deep equivalence.
#' @param obj1 A list of hashable RS objects.
#' @param obj2 Another list of RS objects to compare against.
#' @return `TRUE` if the sets are equal in content and order-independent; otherwise `FALSE`.
    areEqualSets = function(obj1, obj2) {
      if (length(obj1) != length(obj2)) return(FALSE)
      obj1_hashes <- sort(self$hashesOfEachObject(obj1))
      obj2_hashes <- sort(self$hashesOfEachObject(obj2))
      if (!identical(obj1_hashes, obj2_hashes)) return(FALSE)
      differs_by_method <- any(vapply(
        obj1,
        function(o1) {
          o1_hash <- o1$hash()
          !any(vapply(obj2, function(o2) {
            o1_hash == o2$hash() && isTRUE(o1$isEqual(o2))
          }, logical(1)))
        },
        logical(1)
      ))
      return(!differs_by_method)
    },



#' @describeIn RSHashOperator
#' Returns the subset of objects in `obj1` that are also present in `obj2`.
#' Matching is determined via both hash equality and `isEqual()`.
#' @param obj1 A list of RS objects to filter.
#' @param obj2 A list to match against.
#' @return A list of objects from `obj1` that are also found in `obj2`.
    objectsAlsoIn = function(obj1, obj2) {
      obj2_hashes <- self$hashesOfEachObject(obj2)
      Filter(
        function(o1) {
          o1_hash <- o1$hash()
          if (!(o1_hash %in% obj2_hashes)) return(FALSE)
          any(vapply(
            obj2,
            function(o2) {
              isTRUE(o1_hash == o2$hash() && o1$isEqual(o2))
            },
            logical(1)
          ))
        },
        obj1
      )
    },



#' @describeIn RSHashOperator
#' Returns the subset of objects in `obj1` that are not present in `obj2`.
#' Uses hash filtering for early rejection and `isEqual()` for final match checking.
#' @param obj1 A list of RS objects to be filtered.
#' @param obj2 A list of RS objects used as the exclusion base.
#' @return A list of objects in `obj1` not found in `obj2`.
    objectsNotIn = function(obj1, obj2) {
      obj2_hashes <- self$hashesOfEachObject(obj2)
      Filter(
        function(o1) {
          o1_hash <- o1$hash()
          # Step 1: early reject via hash mismatch
          if (!(o1_hash %in% obj2_hashes)) return(TRUE)
          # Step 2: confirm absence by strict isEqual check
          !any(vapply(
            obj2, function(o2) o1$isEqual(o2), logical(1)
          ))
        },
        obj1
      )
    },



#' @describeIn RSHashOperator
#' Returns the union of two sets, combining all unique elements from `obj1` and `obj2`.
#' This is a non-destructive operation: the result includes all elements of `obj2`,
#' plus elements of `obj1` not found in `obj2`.
#' @param obj1 A list of RS objects.
#' @param obj2 A list of RS objects.
#' @return A list representing the union of both sets.
    unionOfSets = function(obj1, obj2) {
      c(obj2, self$objectsNotIn(obj1, obj2))
    },



#' @describeIn RSHashOperator
#' Returns the intersection of two sets — i.e., elements common to both `obj1` and `obj2`.
#' Matching is based on both hash and `isEqual()` checks.
#' @param obj1 A list of RS objects.
#' @param obj2 A list of RS objects.
#' @return A list of objects present in both `obj1` and `obj2`.
    intersectionOfSets = function(obj1, obj2) {
      self$objectsAlsoIn(obj1, obj2)
    },



#' @describeIn RSHashOperator
#' Returns the difference of two sets — i.e., elements in `obj1` that are not in `obj2`.
#' Functionally equivalent to `objectsNotIn()`.
#' @param obj1 A list of RS objects.
#' @param obj2 A list of RS objects to exclude.
#' @return A list of objects in `obj1` but not in `obj2`.
    differenceOfSets = function(obj1, obj2) {
      self$objectsNotIn(obj1, obj2)
    }
  )
)
register_rs_module(RSHashOperator, RSHashOperator$new())