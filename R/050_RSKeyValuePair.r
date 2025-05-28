#' @title Abstract Class for Key-Value Pair
#'
#' @description
#' `RSKeyValuePair` defines an abstract base class for representing a key-value pair structure
#' used in robjc's dictionary-like containers. This class provides the minimal core logic for
#' key-value encapsulation, including accessors and equality/hash operations based solely on the key.
#'
#' Subclasses of `RSKeyValuePair` are responsible for enforcing type or protocol constraints.
#' For example:
#' - `RSClassReference` requires the key to conform to the `RSEncoding` protocol.
#'
#' This class itself does not perform any protocol checks or validation; it simply stores and compares keys and values.
#'
#' @section Inheritance:
#' This class inherits from `RSObject` and is not intended to be instantiated directly.
#'
#' @section Fields:
#' - `key()`: Returns the key object (read-only).
#' - `value()`: Returns the value object (read-only).
#'
#' @section Methods:
#' - `isEqual(other)`: Compares equality based on the key only.
#' - `hash()`: Computes a hash value derived from the key.
#'
#' @return An abstract class definition. Do not instantiate directly.
#'
#' @seealso [RSKeyValuePair], [RSClassReference]
#'
#' @keywords internal
RSKeyValuePair <- R6::R6Class(
  "RSKeyValuePair",
  inherit = RSObject,
  cloneable = FALSE,
  private   = list(
    .key    = NULL,
    .value  = NULL
  ),
  public = list(


    #' @description
    #' Constructs a new key-value pair object. Both key and value must inherit from `RSObject`.
    #'
    #' @param key An `RSObject` to be used as the key.
    #' @param value An `RSObject` to be associated as the value.
    #' @return A new `RSKeyValuePair` instance.
    initialize = function(key, value) {
      stopifnot("Key is not kind of RSObject." = is_robjc_instance(key))
      stopifnot("Value is not kind of RSObject." = is_robjc_instance(value))
      super$initialize()
      private$.key <- key
      private$.value <- value
      invisible(self)
    },




    #' @description
    #' Returns the key object associated with this pair.
    #' @return An `RSObject` representing the key.
    key = function() private$.key,



    #' @description
    #' Returns the value object associated with this pair.
    #' @return An `RSObject` representing the value.
    value = function() private$.value,




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
    hash = function() {
      private$.key$hash()
    }
  )
)