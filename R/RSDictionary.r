#' @include api_setup.r
#' @include RSConcreteDictionary.r
#' @include RSCopying.r
#' @include RSMutableCopying.r
#' @include RSEncoding.r
#'
#' @title RSDictionary: Native-Backed Immutable Key-Value Store
#'
#' @description
#' `RSDictionary` is an immutable, unordered key-value container. Keys and
#' values are RObjC objects, class generators, or protocol generators, with
#' R `NULL` is rejected; use `RSNull$null` for a null key or value. [RSConcreteDictionary] provides native
#' storage and uses each key's `hash()` and `isEqual()` methods.
#'
#' Insertion copies an RObjC object key; class and protocol generators are
#' retained by identity. A stored key's hash and equality-relevant state must
#' remain stable. `copy()` returns the receiver, while `mutableCopy()` creates
#' independent native storage that initially references the same entries.
#'
#' @section Supported Initializations:
#' - `key_value_pairs`: a list of [RSKeyValuePair] objects.
#' - `objects_and_keys`: alternating values and keys.
#' - `keys` and `values`: separate collections of equal length.
#'
#' @section Convenience Constructor:
#' [RSDictionary_withDictionary()] creates an immutable shallow copy from a
#' mutable dictionary. An immutable dictionary is returned unchanged.
#'
#' @export
RSDictionary <- RSObjectClass(
  "RSDictionary",
  version = "0.1.1 beta",
  super_class = RSObject,
  protocols = list(RSMutableCopying, RSCopying, RSCoding),
  private = list(
    .storage = NULL,

    .wrapValue = function(value) {
      if (is.null(value))
        stop("R NULL is not a dictionary value; use RSNull$null or robjc_wrap_null(NULL).",
             call. = FALSE)
      if (robjc_isInstance(value) ||
          robjc_isClass(value) ||
          robjc_isProtocol(value)) return(value)
      stop(sprintf("Unsupported value type: %s", class(value)[1]), call. = FALSE)
    },

    .storeObjectForKey = function(value, key) {
      key <- .robjc_collection_object(key)
      value <- private$.wrapValue(value)
      if (robjc_isInstance(key)) key <- key$copy()
      if (!robjc_isInstance(key) && !robjc_isMetaClass(key))
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
    #' Constructs an immutable dictionary from one supported input format.
    #' With no arguments, constructs an empty dictionary.
    #' @param key_value_pairs A list of [RSKeyValuePair] objects.
    #' @param objects_and_keys Alternating values and keys, for example
    #'   `list(value0, key0, value1, key1)`.
    #' @param keys A collection of keys supplied together with `values`.
    #' @param values A collection of values supplied together with `keys`.
    #' @return A new `RSDictionary` instance.
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
            robjc_isInstance(pair) && pair$isKindOf(RSKeyValuePair)
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

    #' @description Returns the value associated with a key.
    #' @param key An RObjC object, class or protocol generator. R `NULL` is rejected.
    #' @return The stored object, or `NULL` if the key is absent.
    objectForKey = function(key) {
      private$.storage$get(.robjc_collection_object(key))
    },

    #' @description Tests whether a key is present.
    #' @param key An RObjC object, class or protocol generator. R `NULL` is rejected.
    #' @return A logical scalar.
    containsKey = function(key) {
      private$.storage$contains(.robjc_collection_object(key))
    },

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
    #' @param object An RObjC object, class or protocol generator. Use `RSNull$null` for null.
    #' @return A list of matching keys in unspecified order.
    allKeysForObjectAsList = function(object) {
      object <- private$.wrapValue(object)
      entries <- private$.storage$entries()
      matching <- vapply(
        entries,
        function(entry) {
          value <- entry[[2L]]
          if (robjc_isInstance(object))
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
    #' @param object An RObjC object, class or protocol generator. Use `RSNull$null` for null.
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

    #' @description Returns the receiver because it is immutable.
    #' @return `self`.
    copy = function() self,

    #' @description Returns an independent mutable shallow copy.
    #' @return A new `RSMutableDictionary`.
    mutableCopy = function() {
      .rs_dictionary_with_storage(
        RSMutableDictionary,
        .rs_dictionary_storage(self)$duplicate()
      )
    },

    #' @description Computes an order-independent hash from the dictionary keys.
    #' The final result passes through [robjc_normalize_hash()].
    #' @return One non-missing integer hash value.
    hash = function() robjc_normalize_hash(private$.storage$contentHash()),

    #' @description Compares two dictionaries by keys and values.
    #' @param other Another dictionary.
    #' @return `TRUE` when both mappings are equal.
    isEqual = function(other) {
      if (!robjc_isInstance(other) ||
          !other$isKindOf(RSDictionary) ||
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
          robjc_isInstance(other) && other$isKindOf(RSDictionary)
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
          robjc_isInstance(other) && other$isKindOf(RSDictionary)
      )
      result <- self$mutableCopy()
      for (key in other$allKeysAsList()) result$removeObjectForKey(key)
      result$copy()
    },

    #' @description Returns entry objects for legacy serialization hooks.
    #'   Keyed archives use `encodeWithCoder()` for stored pairs.
    #' @return A list of [RSKeyValuePair] objects.
    objectsForEncoding = function() self$allEntriesAsList(),

    #' @description Captures native storage for R serialization.
    #' @return `self`, invisibly.
    prepareForSerialization = function() {
      private$.storage$prepareForSerialization()
      invisible(self)
    },

    #' @description Encodes the stored key and value references as pairs.
    #' @param coder An [RSKeyedArchiver] recording keyed values.
    #' @return `self`, invisibly.
    encodeWithCoder = function(coder) {
      coder$encodeObjectForKey(private$.storage$entries(), "entries")
      invisible(self)
    },

    #' @description Restores native storage from saved pairs without copying
    #'   keys again. This preserves their references for both dictionary classes.
    #' @param coder An [RSKeyedUnarchiver] reading keyed values.
    #' @return `self`, invisibly.
    initWithCoder = function(coder) {
      entries <- coder$decodeObjectForKey("entries")
      if (
        !is.list(entries) ||
        !all(vapply(
          entries,
          function(entry) {
            is.list(entry) && length(entry) == 2L &&
            (robjc_isInstance(entry[[1L]]) || robjc_isMetaClass(entry[[1L]])) &&
            (robjc_isInstance(entry[[2L]]) || robjc_isMetaClass(entry[[2L]]))
          },
          logical(1))
        )
      )
        stop("Decoded dictionary entries must contain RObjC keys and values.",
             call. = FALSE)
      storage <- RSConcreteDictionary$new()
      for (entry in entries) {
        key <- entry[[1L]]
        hash <- key$hash()
        if (!is.integer(hash) || length(hash) != 1L || is.na(hash))
          stop("Key hash() must return one non-missing integer.", call. = FALSE)
        storage$set(key, entry[[2L]])
      }
      private$.storage <- storage
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
