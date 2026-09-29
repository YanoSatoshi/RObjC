#' @title RSMutableDictionary: Native-Backed Mutable Key-Value Store
#'
#' @description
#' `RSMutableDictionary` is a mutable, unordered key-value container. Keys and
#' values are `RSObject` instances, with R `NULL` represented by [RSNull].
#' Storage is delegated to the internal [RSConcreteDictionary] adapter.
#'
#' Keys are copied when inserted through `setObjectForKey()`. Copies of a
#' dictionary duplicate the native table but continue to reference the same
#' key and value objects.
#'
#' @section Supported Initializations:
#' - `key_value_pairs`: a list of [RSKeyValuePair] objects.
#' - `objects_and_keys`: alternating values and keys.
#' - `keys` and `values`: separate collections of equal length.
#'
#' @section Convenience Constructor:
#' `RSMutableDictionary$dictionaryWithDictionary(dictionary)` creates a mutable
#' shallow copy with an independent native table.
#'
#' @export
RSMutableDictionary <- R6::R6Class(
  "RSMutableDictionary",
  cloneable = FALSE,
  inherit = RSObject,
  private = list(
    .storage = NULL,

    .wrapNull = function(object) {
      if (is.null(object)) return(RSNull$null())
      if (is_robjc_instance(object)) return(object)
      stop(sprintf("Unsupported object type: %s", class(object)[1]))
    },

    .storeObjectForKey = function(value, key, copy_key = TRUE) {
      key <- private$.wrapNull(key)
      value <- private$.wrapNull(value)
      if (copy_key) key <- key$copy()
      private$.storage$set(key, value)
      invisible(self)
    }
  ),
  public = list(
    #' @description
    #' Constructs a mutable dictionary from one supported input format.
    #' With no arguments, constructs an empty dictionary.
    #' @param key_value_pairs A list of [RSKeyValuePair] objects.
    #' @param objects_and_keys Alternating values and keys, for example
    #'   `list(value0, key0, value1, key1)`.
    #' @param keys A collection of keys supplied together with `values`.
    #' @param values A collection of values supplied together with `keys`.
    #' @return A new `RSMutableDictionary` instance.
    initialize = function(
      key_value_pairs = NULL,
      objects_and_keys = NULL,
      keys = NULL,
      values = NULL
    ) {
      has_pairs <- !is.null(key_value_pairs)
      has_alternating <- !is.null(objects_and_keys)
      has_keys <- !is.null(keys)
      has_values <- !is.null(values)
      has_separate <- has_keys && has_values

      if (xor(has_keys, has_values)) {
        stop("Keys and values must be provided together.")
      }
      if (sum(has_pairs, has_alternating, has_separate) > 1L) {
        stop(paste(
          "Choose one initialization format:",
          "key_value_pairs, objects_and_keys, or keys and values."
        ))
      }

      super$initialize()
      private$.storage <- RSConcreteDictionary$new()

      if (has_alternating) {
        objects_and_keys <- cast_rs_storage_to_list(objects_and_keys)
        if (length(objects_and_keys) %% 2L != 0L) {
          stop("Length of 'objects_and_keys' must be even.")
        }
        positions <- seq_along(objects_and_keys)
        values <- objects_and_keys[positions %% 2L == 1L]
        keys <- objects_and_keys[positions %% 2L == 0L]
        has_separate <- TRUE
      }

      if (has_separate) {
        keys <- cast_rs_storage_to_list(keys)
        values <- cast_rs_storage_to_list(values)
        if (length(keys) != length(values)) {
          stop("Keys and values must have same length.")
        }
        for (i in seq_along(keys)) {
          private$.storeObjectForKey(values[[i]], keys[[i]])
        }
      }

      if (has_pairs) {
        key_value_pairs <- cast_rs_storage_to_list(key_value_pairs)
        valid <- vapply(
          key_value_pairs,
          function(pair) {
            is_robjc_instance(pair) && pair$isKindOf(RSKeyValuePair)
          },
          logical(1)
        )
        if (!all(valid)) {
          stop("InvalidArgumentError. 'key_value_pairs' are contaminated!")
        }
        for (entry in key_value_pairs) {
          private$.storeObjectForKey(entry$value(), entry$key())
        }
      }

      invisible(self)
    },

    #' @description Inserts or replaces a value for a key.
    #' @param value An `RSObject` value, or `NULL`.
    #' @param key An `RSObject` key, or `NULL`.
    #' @return `self`, invisibly.
    setObjectForKey = function(value, key) {
      private$.storeObjectForKey(value, key)
    },

    #' @description Returns the value associated with a key.
    #' @param key An `RSObject` key, or `NULL`.
    #' @return The stored object, or `NULL` if the key is absent.
    objectForKey = function(key) {
      private$.storage$get(private$.wrapNull(key))
    },

    #' @description Tests whether a key is present.
    #' @param key An `RSObject` key, or `NULL`.
    #' @return A logical scalar.
    containsKey = function(key) {
      private$.storage$contains(private$.wrapNull(key))
    },

    #' @description Removes the value associated with a key, if present.
    #' @param key An `RSObject` key, or `NULL`.
    #' @return `self`, invisibly.
    removeObjectForKey = function(key) {
      private$.storage$remove(private$.wrapNull(key))
      invisible(self)
    },

    #' @description Removes every entry.
    #' @return `self`, invisibly.
    removeAllObjects = function() {
      private$.storage$clear()
      invisible(self)
    },

    #' @description Adds or replaces an [RSKeyValuePair].
    #' @param entry An `RSKeyValuePair` object.
    #' @return `self`, invisibly.
    addEntry = function(entry) {
      stopifnot(
        "Entry must be a kind of RSKeyValuePair" =
          is_robjc_instance(entry) && entry$isKindOf(RSKeyValuePair)
      )
      private$.storeObjectForKey(entry$value(), entry$key(), copy_key = FALSE)
    },

    #' @description Removes the entry with the supplied pair's key.
    #' @param entry An `RSKeyValuePair` object.
    #' @return `self`, invisibly.
    removeEntry = function(entry) {
      stopifnot(
        "Entry must be a kind of RSKeyValuePair" =
          is_robjc_instance(entry) && entry$isKindOf(RSKeyValuePair)
      )
      self$removeObjectForKey(entry$key())
    },

    #' @description Removes every entry.
    #' @return `self`, invisibly.
    removeAllEntries = function() self$removeAllObjects(),

    #' @description Returns all entries as [RSKeyValuePair] objects.
    #' @return A list in unspecified order.
    asList = function() self$allEntriesAsList(),

    #' @description Returns all entries as [RSKeyValuePair] objects.
    #' @return A list in unspecified order.
    allEntriesAsList = function() {
      keys <- private$.storage$keys()
      values <- private$.storage$values()
      Map(RSKeyValuePair$new, keys, values)
    },

    #' @description Returns all entries as an [RSArray].
    #' @return An `RSArray`.
    allEntries = function() RSArray$new(self$allEntriesAsList()),

    #' @description Returns all keys as a list.
    #' @return A list in unspecified order.
    allKeysAsList = function() private$.storage$keys(),

    #' @description Returns all keys as an [RSArray].
    #' @return An `RSArray`.
    allKeys = function() RSArray$new(self$allKeysAsList()),

    #' @description Returns all values as a list.
    #' @return A list corresponding to `allKeysAsList()` when unmodified.
    allValuesAsList = function() private$.storage$values(),

    #' @description Returns all values as an [RSArray].
    #' @return An `RSArray`.
    allValues = function() RSArray$new(self$allValuesAsList()),

    #' @description Alias for `allValuesAsList()`.
    #' @return A list of values.
    allObjectsAsList = function() self$allValuesAsList(),

    #' @description Alias for `allValues()`.
    #' @return An `RSArray` of values.
    allObjects = function() self$allValues(),

    #' @description Returns the number of entries.
    #' @return A non-negative numeric scalar.
    count = function() private$.storage$count(),

    #' @description Returns an immutable shallow copy.
    #' @return A new [RSDictionary].
    copy = function() RSDictionary$dictionaryWithDictionary(self),

    #' @description Returns an independent mutable shallow copy.
    #' @return A new `RSMutableDictionary`.
    mutableCopy = function() {
      RSMutableDictionary$dictionaryWithDictionary(self)
    },

    #' @description Computes an order-independent hash from the dictionary keys.
    #' @return An integer hash value.
    hash = function() private$.storage$contentHash(),

    #' @description Compares two dictionaries by keys and values.
    #' @param other Another dictionary.
    #' @return `TRUE` when both mappings are equal.
    isEqual = function(other) {
      if (!is_robjc_instance(other) ||
          !other$isKindOf(RSMutableDictionary) ||
          self$count() != other$count()) {
        return(FALSE)
      }
      for (key in self$allKeysAsList()) {
        if (!other$containsKey(key)) return(FALSE)
        if (!isTRUE(self$objectForKey(key)$isEqual(other$objectForKey(key)))) {
          return(FALSE)
        }
      }
      TRUE
    },

    #' @description Returns a concise textual representation.
    #' @param ... Ignored.
    #' @return A character scalar.
    description = function(...) {
      paste(super$description(), sprintf("%d entries", self$count()))
    },

    #' @description Returns an immutable merge; values from `other` win.
    #' @param other Another dictionary.
    #' @return A new [RSDictionary].
    dictionaryByMergingWithDictionary = function(other) {
      stopifnot(
        "Other must be a dictionary" =
          is_robjc_instance(other) && other$isKindOf(RSMutableDictionary)
      )
      merged <- self$mutableCopy()
      for (entry in other$allEntriesAsList()) merged$addEntry(entry)
      merged$copy()
    },

    #' @description Returns an immutable copy excluding keys found in `other`.
    #' @param other Another dictionary.
    #' @return A new [RSDictionary].
    dictionaryByExcludingWithDictionary = function(other) {
      stopifnot(
        "Other must be a dictionary" =
          is_robjc_instance(other) && other$isKindOf(RSMutableDictionary)
      )
      result <- self$mutableCopy()
      for (key in other$allKeysAsList()) result$removeObjectForKey(key)
      result$copy()
    },

    #' @description Returns the dictionary entries required for encoding.
    #' @return A list of [RSKeyValuePair] objects.
    objectsForEncoding = function() self$allEntriesAsList(),

    #' @description Captures native storage for R serialization.
    #' @return `self`, invisibly.
    prepareForSerialization = function() {
      private$.storage$prepareForSerialization()
      invisible(self)
    }
  )
)
conform_protocol_to_class(RSMutableCopying$protocol(), RSMutableDictionary)
conform_protocol_to_class(RSCopying$protocol(), RSMutableDictionary)


.rs_dictionary_storage <- function(dictionary) {
  dictionary$.__enclos_env__$private$.storage
}

.rs_dictionary_with_storage <- function(generator, storage) {
  stopifnot(inherits(storage, "RSConcreteDictionary"))
  result <- generator$new()
  result$.__enclos_env__$private$.storage <- storage
  result
}

# Creates a mutable dictionary with independent native storage.
RSMutableDictionary$dictionaryWithDictionary <- function(dictionary) {
  stopifnot(
    "Object must be a dictionary" =
      is_robjc_instance(dictionary) &&
      dictionary$isKindOf(RSMutableDictionary)
  )
  .rs_dictionary_with_storage(
    RSMutableDictionary,
    .rs_dictionary_storage(dictionary)$duplicate()
  )
}
