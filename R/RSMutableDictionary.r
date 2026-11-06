#' @include RSDictionary.r
#' @title RSMutableDictionary: Native-Backed Mutable Key-Value Store
#'
#' @description
#' `RSMutableDictionary` extends [RSDictionary] with operations to insert,
#' replace, and remove entries. It shares the native storage, lookup, equality,
#' coding, and derived dictionary operations of its parent.
#'
#' Insertion copies an RObjC object key according to [RSCopying]. Class and
#' protocol generator keys are retained by identity. Keep a stored key's
#' hash- and equality-relevant state unchanged while it remains in the
#' dictionary. `copy()` returns an independent immutable [RSDictionary].
#'
#' @section Protocols:
#' - [RSMutableCopying]
#' - [RSCopying]
#' - [RSListing]
#' - [RSCoding]
#'
#' @section Convenience Constructor:
#' [RSMutableDictionary_withDictionary()] creates a mutable shallow copy with
#' an independent native table.
#'
#' @export
RSMutableDictionary <- RSObjectClass(
  "RSMutableDictionary",
  version = "0.1.1 beta",
  super_class = RSDictionary,
  protocols = list(RSMutableCopying, RSCopying, RSListing, RSCoding),
  public = list(
    #' @description Constructs a mutable dictionary from one supported input format.
    #' @param key_value_pairs A list of [RSKeyValuePair] objects.
    #' @param objects_and_keys Alternating values and keys.
    #' @param keys A collection of keys supplied together with `values`.
    #' @param values A collection of values supplied together with `keys`.
    #' @return A new `RSMutableDictionary` instance.
    initialize = function(
      key_value_pairs   = NULL,
      objects_and_keys  = NULL,
      keys              = NULL,
      values            = NULL
    ) {
      super$initialize(
        key_value_pairs   = key_value_pairs,
        objects_and_keys  = objects_and_keys,
        keys              = keys,
        values            = values
      )
      invisible(self)
    },



    #' @description Inserts or replaces a value for a key.
    #' @param value An RObjC object, class generator, or protocol generator. Use `RSNull$null` for null.
    #' @param key An RObjC object key with a valid integer `hash()`, a class or
    #'   protocol generator. Use `RSNull$null` for a null key.
    #' @return `self`, invisibly.
    setObjectForKey = function(value, key) private$.storeObjectForKey(value, key),



    #' @description Removes the value associated with a key, if present.
    #' @param key An RObjC object, class or protocol generator. R `NULL` is rejected.
    #' @return `self`, invisibly.
    removeObjectForKey = function(key) {
      private$.storage$remove(.robjc_collection_object(key))
      invisible(self)
    },



    #' @description Removes every entry.
    #' @return `self`, invisibly.
    removeAllObjects = function() {
      private$.storage$clear()
      invisible(self)
    },



    #' @description Adds or replaces an [RSKeyValuePair], calling `copy()` on an
    #'   RObjC object key and retaining a generator key by identity.
    #' @param entry An `RSKeyValuePair` object.
    #' @return `self`, invisibly.
    addEntry = function(entry) {
      stopifnot(
        "Entry must be a kind of RSKeyValuePair" =
          robjc_isInstance(entry) && entry$isKindOf(RSKeyValuePair)
      )
      private$.storeObjectForKey(entry$value(), entry$key())
    },



    #' @description Removes the entry with the supplied pair's key.
    #' @param entry An `RSKeyValuePair` object.
    #' @return `self`, invisibly.
    removeEntry = function(entry) {
      stopifnot(
        "Entry must be a kind of RSKeyValuePair" =
          robjc_isInstance(entry) && entry$isKindOf(RSKeyValuePair)
      )
      self$removeObjectForKey(entry$key())
    },



    #' @description Removes every entry.
    #' @return `self`, invisibly.
    removeAllEntries = function() self$removeAllObjects(),



    #' @description Returns an immutable shallow copy.
    #' @return A new [RSDictionary] with independent native storage.
    copy = function() {
      .rs_dictionary_with_storage(
        RSDictionary,
        .rs_dictionary_storage(self)$duplicate()
      )
    }
  )
)
