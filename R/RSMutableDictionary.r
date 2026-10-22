#' @include api_setup.r
#' @include RSConcreteDictionary.r
#' @include RSCopying.r
#' @include RSMutableCopying.r
#' 
#' @title RSMutableDictionary: Native-Backed Mutable Key-Value Store
#'
#' @description
#' `RSMutableDictionary` is a mutable, unordered key-value container. Keys and
#' values are RObjC objects, class generators, or protocol generators, with
#' R `NULL` represented by [RSNull].
#' Storage is delegated to the internal [RSConcreteDictionary] adapter.
#'
#' For an RObjC object key, insertion calls `key$copy()` in accordance with
#' [RSCopying]. The returned object may be the original key: [RSObject]'s
#' default `copy()` returns `self`. Class and protocol generators are retained
#' by identity. Keep a stored key's hash- and equality-relevant state unchanged
#' while it remains in the dictionary. Copies of a dictionary duplicate the
#' native table but continue to reference the same key and value objects.
#' A key's `hash()` is checked before native insertion and must return one
#' nonmissing integer.
#'
#' @section Supported Initializations:
#' - `key_value_pairs`: a list of [RSKeyValuePair] objects.
#' - `objects_and_keys`: alternating values and keys.
#' - `keys` and `values`: separate collections of equal length.
#'
#' @section Convenience Constructor:
#' [RSMutableDictionary_withDictionary()] creates a mutable shallow copy with
#' an independent native table.
#'
#' @export
RSMutableDictionary <- RSObjectClass(
  "RSMutableDictionary",
  version = "0.0.4 alpha",
  super_class = RSObject,
  protocols   = list(RSMutableCopying, RSCopying),
  private = list(
    .storage = NULL,

    .wrapNull = function(object) {
      if (is.null(object)) return(RSNull$null)
      if (robjc_isRobjcObject(object) || robjc_isMetaClass(object)) return(object)
      stop(sprintf("Unsupported object type: %s", class(object)[1]))
    },

    .wrapValue = function(value) {
      if (is.null(value)) return(RSNull$null)
      if (robjc_isRobjcObject(value) ||
          robjc_isClass(value) ||
          robjc_isProtocol(value)) return(value)
      stop(sprintf("Unsupported value type: %s", class(value)[1]), call. = FALSE)
    },

    .storeObjectForKey = function(value, key) {
      key <- private$.wrapNull(key)
      value <- private$.wrapValue(value)
      if (robjc_isRobjcObject(key)) key <- key$copy()
      if (!robjc_isRobjcObject(key) && !robjc_isMetaClass(key))
        stop("Copied key must be an RObjC object, class, or protocol.", call. = FALSE)
      hash <- key$hash()
      if (!is.integer(hash) || length(hash) != 1L || is.na(hash))
        stop("Key hash() must return one non-missing integer.", call. = FALSE)
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
        objects_and_keys <- cast_collections_to_list(objects_and_keys)
        if (length(objects_and_keys) %% 2L != 0L) {
          stop("Length of 'objects_and_keys' must be even.")
        }
        positions <- seq_along(objects_and_keys)
        values <- objects_and_keys[positions %% 2L == 1L]
        keys <- objects_and_keys[positions %% 2L == 0L]
        has_separate <- TRUE
      }

      if (has_separate) {
        keys <- cast_collections_to_list(keys)
        values <- cast_collections_to_list(values)
        if (length(keys) != length(values)) {
          stop("Keys and values must have same length.")
        }
        for (i in seq_along(keys)) {
          private$.storeObjectForKey(values[[i]], keys[[i]])
        }
      }

      if (has_pairs) {
        key_value_pairs <- cast_collections_to_list(key_value_pairs)
        valid <- vapply(
          key_value_pairs,
          function(pair) {
            robjc_isRobjcObject(pair) && pair$isKindOf(RSKeyValuePair)
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
    #' @param value An RObjC object, class generator, protocol generator, or `NULL`.
    #' @param key An RObjC object key with a valid integer `hash()`, a class or
    #'   protocol generator, or `NULL`.
    #' @return `self`, invisibly.
    setObjectForKey = function(value, key) {
      private$.storeObjectForKey(value, key)
    },

    #' @description Returns the value associated with a key.
    #' @param key An RObjC object, class or protocol generator, or `NULL`.
    #' @return The stored object, or `NULL` if the key is absent.
    objectForKey = function(key) {
      private$.storage$get(private$.wrapNull(key))
    },

    #' @description Tests whether a key is present.
    #' @param key An RObjC object, class or protocol generator, or `NULL`.
    #' @return A logical scalar.
    containsKey = function(key) {
      private$.storage$contains(private$.wrapNull(key))
    },

    #' @description Removes the value associated with a key, if present.
    #' @param key An RObjC object, class or protocol generator, or `NULL`.
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

    #' @description Adds or replaces an [RSKeyValuePair], calling `copy()` on an
    #'   RObjC object key and retaining a generator key by identity.
    #' @param entry An `RSKeyValuePair` object.
    #' @return `self`, invisibly.
    addEntry = function(entry) {
      stopifnot(
        "Entry must be a kind of RSKeyValuePair" =
          robjc_isRobjcObject(entry) && entry$isKindOf(RSKeyValuePair)
      )
      private$.storeObjectForKey(entry$value(), entry$key())
    },

    #' @description Removes the entry with the supplied pair's key.
    #' @param entry An `RSKeyValuePair` object.
    #' @return `self`, invisibly.
    removeEntry = function(entry) {
      stopifnot(
        "Entry must be a kind of RSKeyValuePair" =
          robjc_isRobjcObject(entry) && entry$isKindOf(RSKeyValuePair)
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
      lapply(
        private$.storage$entries(),
        function(entry) RSKeyValuePair$new(entry[[1L]], entry[[2L]])
      )
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

    #' @description Returns every key whose value is equal to `object`.
    #' @param object An RObjC object, class or protocol generator, or `NULL` for [RSNull].
    #' @return A list of matching keys in unspecified order.
    allKeysForObjectAsList = function(object) {
      object <- private$.wrapValue(object)
      entries <- private$.storage$entries()
      matching <- vapply(
        entries,
        function(entry) {
          value <- entry[[2L]]
          if (robjc_isRobjcObject(object))
            isTRUE(object$isEqual(value))
          else if (robjc_isMetaClass(object))
            identical(object, value)
          else
            FALSE
        },
        logical(1)
      )
      lapply(entries[matching], function(entry) entry[[1L]])
    },

    #' @description Returns every key whose value is equal to `object`.
    #' @param object An RObjC object, class or protocol generator, or `NULL` for [RSNull].
    #' @return An [RSArray] of matching keys in unspecified order.
    allKeysForObject = function(object) {
      RSArray$new(self$allKeysForObjectAsList(object))
    },

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
    copy = function() {
      .rs_dictionary_with_storage(
        RSDictionary,
        .rs_dictionary_storage(self)$duplicate()
      )
    },

    #' @description Returns an independent mutable shallow copy.
    #' @return A new `RSMutableDictionary`.
    mutableCopy = function() {
      .rs_dictionary_with_storage(
        RSMutableDictionary,
        .rs_dictionary_storage(self)$duplicate()
      )
    },

    #' @description Computes an order-independent hash from the dictionary keys.
    #' The final result passes through [normalize_hash()].
    #' @return One non-missing integer hash value.
    hash = function() normalize_hash(private$.storage$contentHash()),

    #' @description Compares two dictionaries by keys and values.
    #' @param other Another dictionary.
    #' @return `TRUE` when both mappings are equal.
    isEqual = function(other) {
      if (!robjc_isRobjcObject(other) ||
          !other$isKindOf(RSMutableDictionary) ||
          self$count() != other$count()) {
        return(FALSE)
      }
      all(vapply(
        self$allKeysAsList(),
        function(key) {
          isTRUE(other$containsKey(key)) &&
            isTRUE(self$objectForKey(key)$isEqual(other$objectForKey(key)))
        },
        logical(1)
      ))
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
          robjc_isRobjcObject(other) && other$isKindOf(RSMutableDictionary)
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
          robjc_isRobjcObject(other) && other$isKindOf(RSMutableDictionary)
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

.rs_dictionary_storage <- function(dictionary) {
  dictionary$.__enclos_env__$private$.storage
}

.rs_dictionary_with_storage <- function(generator, storage) {
  stopifnot(inherits(storage, "RSConcreteDictionary"))
  result <- generator$new()
  result$.__enclos_env__$private$.storage <- storage
  result
}
