#' @title RSKeyValuePair: Key-Value Object Wrapper for Hash-Based Containers
#'
#' @description
#' `RSKeyValuePair` is a simple immutable object that wraps a key-value pair.
#' It is primarily used internally by hash-based containers such as `RSDictionary`
#' to manage object associations. Both the key and value must inherit from `RSObject`.
#'
#' This class supports hashing and equality checks based solely on the key,
#' making it suitable for use in hash tables and set-like structures where
#' key uniqueness is enforced.
#'
#' @section Core Methods:
#' - `key()` — Returns the key object.
#' - `value()` — Returns the value object.
#' - `isEqual()` — Compares keys only.
#' - `hash()` — Computes hash based on the key.
#'
#' @examples
#' key <- RSString$stringWith("id")
#' val <- RSNumber$new(42)
#' pair <- RSKeyValuePair$new(key, val)
#' pair$key()$value()   # "id"
#' pair$value()$value() # 42
#'
#' @export
RSKeyValuePair <- R6::R6Class(
  ### class name ###
  "RSKeyValuePair",
  cloneable = FALSE,
  inherit   = RSObject,
  public    = list(

#' @description
#' Constructs a new key-value pair object. Both key and value must inherit from `RSObject`.
#'
#' @param key An `RSObject` to be used as the key.
#' @param value An `RSObject` to be associated as the value.
#' @return A new `RSKeyValuePair` instance.
    initialize = function(key, value) {
      super$initialize()
      stopifnot("Key is not kind of RSObject." =
        !is.atomic(key) &&
        !is.null(key$isKindOf) &&
        key$isKindOf(RSObject)
      )
      stopifnot("Value is not kind of RSObject." =
        !is.atomic(value) &&
        !is.null(value$isKindOf) &&
        value$isKindOf(RSObject)
      )
      private$.key   <- key
      private$.value <- value
      invisible(self)
    },



#' @description
#' Returns the key object associated with this pair.
#' @return An `RSObject` representing the key.
    key = function() {
      private$.key
    },



#' @description
#' Returns the value object associated with this pair.
#' @return An `RSObject` representing the value.
    value = function() {
      private$.value
    },



#' @description
#' Compares this pair to another by checking key equivalence.
#' Only the key is considered for equality; the value is ignored.
#'
#' @param other Another `RSKeyValuePair` instance.
#' @return `TRUE` if the keys are equal, otherwise `FALSE`.
    isEqual = function(other) {
      if (!other$isMemberOf(self$class())) return(FALSE)
      isTRUE(private$.key$isEqual(other$key()))
    },



#' @description
#' Computes the hash code based on the key only.
#' This allows the pair to behave correctly in hash-based containers.
#'
#' @return An integer hash value derived from the key.
    hash = function(){
      private$.key$hash()
    }
  ),
  private   = list(
    .key    = NULL,
    .value  = NULL
  )
)
