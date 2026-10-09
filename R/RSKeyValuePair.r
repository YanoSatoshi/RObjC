#' @include api_setup.r
#' @title Key-Value Pair Class
#'
#' @description
#' `RSKeyValuePair` is a concrete class representing a key-value pair used in dictionary-like containers
#' within the `robjc` object system. The key and value must both be instances of `RSObject`.
#'
#' The class provides read-only accessors for the key and value. Equality and hashing are based
#' solely on the key, which allows consistent behavior in hashed containers such as dictionaries or sets.
#'
#' @section Inheritance:
#' Inherits from [`RSObject`]. Unlike earlier versions, this class is now concrete and can be instantiated directly.
#'
#' @section Fields:
#' - `key()`: Returns the key object.
#' - `value()`: Returns the value object.
#'
#' @section Methods:
#' - `setKey(key)`: Replaces the key if the value is `RSNull`. Used during internal construction in mutable dictionaries.
#' - `isEqual(other)`: Compares equality based on the key only.
#' - `hash()`: Computes a hash value derived from the key.
#' - `objectsForEncoding()`: Returns both key and value as a list for serialization.
#'
#' @return A concrete instance of `RSKeyValuePair`.
#'
#' @seealso [RSObject], [RSClassReference]
#'
RSKeyValuePair <- RSObjectClass(
  "RSKeyValuePair",
  version = "0.0.2 alpha",
  super_class = RSObject,
  private     = list(
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
      stopifnot(
        "Key is not kind of RSObject."    = robjc_isRobjcObject(key),
        "Value is not kind of RSObject."  = robjc_isRobjcObject(value)
      )
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
    #' Replaces the key object in this key-value pair.
    #' This operation is only allowed when the value is \code{RSNull}, which signals that the pair is in a reusable placeholder state.
    #'
    #' This method is primarily intended for internal use in dictionary containers.
    #' Dictionaries often reuse a single \code{RSKeyValuePair} instance with an \code{RSNull} value
    #' to perform temporary lookups without allocating a new object each time.
    #'
    #' @param key An \code{RSObject} to be used as the new key.
    #'
    #' @note
    #' The key can only be replaced if the value is \code{RSNull}. Attempting to change the key
    #' when the value is bound to a non-null object will result in an error.
    #'
    #' @examples
    #' # Safe usage for dictionary lookups
    #' pair <- robjc:::RSKeyValuePair$new(RSNull$null, RSNull$null)
    #' pair$setKey(RSString_withValue("some_key"))
    #'
    #' \dontrun{
    #' # This would raise an error
    #' pair <- RSKeyValuePair$new(RSString$new("a"), RSNumber$new(123))
    #' pair$setKey(RSString$new("b"))         # Error: not allowed when value is non-null
    #' }
    setKey = function(key) {
      if (!identical(private$.value, RSNull$null)) {
        stop("Key replacing available only when value is RSNull. RSKeyValuePair")
      }
      private$.key <- key
      invisible(self)
    },



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
    },



    #' @description
    #' Returns encoding objects, key and value
    #'
    #' @return List from key and value
    objectsForEncoding = function() {
      list(private$.key, private$.value)
    }
  )
)
