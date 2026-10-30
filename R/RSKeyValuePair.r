#' @include api_setup.r RSEncoding.r
#' @title Key-Value Pair Class
#'
#' @description
#' `RSKeyValuePair` is a concrete class representing a key-value pair used in dictionary-like containers
#' within the `robjc` object system. Keys and values may be RObjC objects,
#' class generators, or protocol generators.
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
#' - `setKey(key)`: Replaces the key if the value is `RSNull`.
#' - `isEqual(other)`: Compares equality based on the key only.
#' - `hash()`: Computes a hash value derived from the key.
#' - `encodeWithCoder()` and `initWithCoder()`: Preserve the key and value references.
#' - `objectsForEncoding()`: Returns instance members for legacy serialization hooks.
#'
#' @return A concrete instance of `RSKeyValuePair`.
#'
#' @seealso [RSObject], [RSMutableDictionary]
#'
RSKeyValuePair <- RSObjectClass(
  "RSKeyValuePair",
  version = "0.1.0 beta",
  super_class = RSObject,
  protocols = list(RSCoding),
  private     = list(
    .key    = NULL,
    .value  = NULL
  ),
  public = list(

    #' @description
    #' Constructs a new key-value pair with a supported key and value.
    #'
    #' @param key An RObjC object, class generator, or protocol generator.
    #' @param value An RObjC object, class generator, or protocol generator.
    #' @return A new `RSKeyValuePair` instance.
    initialize = function(key, value) {
      stopifnot(
        "Key must be an RObjC object, class, or protocol." =
          robjc_isRobjcObject(key) || robjc_isMetaClass(key),
        "Value must be an RObjC object, class, or protocol." =
          robjc_isRobjcObject(value) || robjc_isClass(value) ||
          robjc_isProtocol(value)
      )
      super$initialize()
      private$.key <- key
      private$.value <- value
      invisible(self)
    },

    #' @description Encodes the key and value as separate references.
    #' @param coder An [RSKeyedArchiver] recording keyed values.
    #' @return `self`, invisibly.
    encodeWithCoder = function(coder) {
      coder$encodeObjectForKey(private$.key, "key")
      coder$encodeObjectForKey(private$.value, "value")
      invisible(self)
    },

    #' @description Restores the key and value without calling `initialize()`.
    #' @param coder An [RSKeyedUnarchiver] reading keyed values.
    #' @return `self`, invisibly.
    initWithCoder = function(coder) {
      key <- coder$decodeObjectForKey("key")
      value <- coder$decodeObjectForKey("value")
      if (!robjc_isRobjcObject(key) && !robjc_isMetaClass(key))
        stop("Decoded key must be an RObjC object, class, or protocol.",
             call. = FALSE)
      if (!robjc_isRobjcObject(value) && !robjc_isMetaClass(value))
        stop("Decoded value must be an RObjC object, class, or protocol.",
             call. = FALSE)
      private$.key <- key
      private$.value <- value
      invisible(self)
    },




    #' @description
    #' Returns the key object associated with this pair.
    #' @return The RObjC object, class generator, or protocol generator key.
    key = function() private$.key,



    #' @description
    #' Replaces the key object in this key-value pair.
    #' This operation is only allowed when the value is \code{RSNull}, which signals that
    #' the pair is in a reusable placeholder state.
    #'
    #' @param key An RObjC object, class generator, or protocol generator.
    #'
    #' @note
    #' The key can only be replaced if the value is \code{RSNull}. Attempting to change the key
    #' when the value is bound to a non-null object will result in an error.
    #'
    #' @examples
    #' # A pair with an RSNull value permits replacing its key.
    #' pair <- robjc:::RSKeyValuePair$new(RSNull$null, RSNull$null)
    #' pair$setKey(RSString_withValue("some_key"))
    #'
    #' \dontrun{
    #' # This would raise an error
    #' pair <- RSKeyValuePair$new(RSString$new("a"), RSNumber$new(123))
    #' pair$setKey(RSString$new("b"))         # Error: not allowed when value is non-null
    #' }
    setKey = function(key) {
      if (!robjc_isRobjcObject(key) && !robjc_isMetaClass(key))
        stop("Key must be an RObjC object, class, or protocol.", call. = FALSE)
      if (!identical(private$.value, RSNull$null)) {
        stop("Key replacing available only when value is RSNull. RSKeyValuePair")
      }
      private$.key <- key
      invisible(self)
    },



    #' @description
    #' Returns the value object associated with this pair.
    #' @return The stored RObjC object, class generator, or protocol generator.
    value = function() private$.value,




    #' @description
    #' Compares this pair to another by checking key equivalence.
    #' Only the key is considered for equality; the value is ignored.
    #'
    #' @param other Another `RSKeyValuePair` instance.
    #' @return `TRUE` if the keys are equal; `FALSE` for other inputs.
    isEqual = function(other) {
      if (!robjc_isRobjcObject(other) || !other$isMemberOf(self$class()))
        return(FALSE)
      isTRUE(private$.key$isEqual(other$key()))
    },



    #' @description
    #' Computes the hash code based on the key only.
    #' This allows the pair to behave correctly in hash-based containers.
    #' The final result passes through [robjc_normalize_hash()].
    #'
    #' @return One non-missing integer hash value derived from the key.
    hash = function() robjc_normalize_hash(private$.key$hash()),



    #' @description
    #' Returns RObjC instance keys and values for legacy serialization hooks.
    #' Keyed archives use `encodeWithCoder()` for all key and value references.
    #'
    #' @return A list of the RObjC instance key and value, when present.
    objectsForEncoding = function() {
      Filter(robjc_isRobjcObject, list(private$.key, private$.value))
    }
  )
)
