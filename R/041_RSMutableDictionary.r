#' @title RSMutableDictionary: Hash-Based Mutable Key-Value Store
#'
#' @description
#' `RSMutableDictionary` is a mutable, unordered key-value container in which both keys
#' and values must be `RSObject`s. Internally, it uses an `RSDynamicHash` containing
#' `RSKeyValuePair` entries. Keys are hashed and compared using `hash()` and `isEqual()`.
#'
#' All keys and values are wrapped via `.wrapNull()` to support `RSNull`, and keys are
#' copied using `copyWithEnv()` to maintain independence of reference.
#'
#' @section Supported Initializations:
#' - `key_value_pairs`: list of `RSKeyValuePair`
#' - `objects_and_keys`: alternating key-value sequence list
#' - `keys` + `values`: paired vectors or lists
#'
#' @export
RSMutableDictionary <- R6::R6Class(
  "RSMutableDictionary",
  cloneable = FALSE,
  inherit = RSObject,
  private = list(
    .hash_table = NULL,
    .copying = NULL,
    .wrapNull = function(some) {
      if (is.null(some)) return(RSNull$null())
      if (!is.null(some$isMemberOf) && some$isKindOf(RSObject)) return(some)
      stop(sprintf("Unsupported key type: %s", class(some)[1]))
    },
    .getEntryByKey = function(entry_key) {
      private$.hash_table$getEntryMatching(entry_key, function(entry) entry$key())
    }
  ),
  public = list(
    #' @description
    #' Constructs a dictionary using one of three formats: a list of key-value pairs,
    #' an alternating key-value list, or separate key and value lists.
    #' Keys and values are wrapped and stored in the internal hash table.
    #'
    #' @param key_value_pairs A list of `RSKeyValuePair` objects.
    #' @param objects_and_keys Alternating list of \code{list(key0, value0, key1, value1, ...)}.
    #' @param keys List or vector of key objects.
    #' @param values List or vector of value objects.
    initialize = function(
      key_value_pairs  = NULL, # default:          list[RSKeyValuePair] # nolint
      objects_and_keys = NULL, # alternative:      list[key0, value0, key1, value1, ...] #nolint
      keys   = NULL,           # give with values: list[key0, key1, ...]
      values = NULL            # give with keys:   list[value0, value1, ...]
    ) {
      is_kv_pair  <- !is.null(key_value_pairs)
      is_alt_list <- !is.null(objects_and_keys)
      is_separate <- !is.null(keys) && !is.null(values)
      if (sum(is_kv_pair, is_alt_list, is_separate) > 1) {
        stop("Choose a initializing by key_value_pairs or objects_and_keys, or 'keys and values'") # nolint
      }
      super$initialize()
      private$.hash_table <- RSDynamicHash$new()
      private$.copying <- RSCopying$protocol()
      if (sum(is_kv_pair, is_alt_list, is_separate) == 0) {
        # returns empty dict
        return(invisible(self))
      }
      if (!is_kv_pair && is_alt_list) {
        objects_and_keys <- cast_ry_storage_to_list(objects_and_keys)
        if (length(objects_and_keys) %% 2 != 0) {
          stop("Length of 'objects_and_keys' must be even.")
        }
        keys   <- objects_and_keys[seq(2, length(objects_and_keys), by = 2)]
        values <- objects_and_keys[seq(1, length(objects_and_keys), by = 2)]
        is_separate <- TRUE
      }
      if (!is_kv_pair && is_separate) {
        keys <- cast_ry_storage_to_list(keys)
        values <- cast_ry_storage_to_list(values)
        if (length(keys) != length(values)) {
          stop("Keys and values must have same length.")
        }
        key_value_pairs <- mapply(
          function(aKey, aValue) {
            RSKeyValuePair$new(
              private$.wrapNull(aKey), private$.wrapNull(aValue)
            )
          },
          keys, values, SIMPLIFY = FALSE
        )
      }
      # finally
      key_value_pairs <- cast_ry_storage_to_list(key_value_pairs)
      if (is.null(key_value_pairs)) return(NULL)
      contamination <- vapply(
        key_value_pairs,
        function(pair) {
          isTRUE(
            pair$isMemberOf(RSKeyValuePair) || pair$isKindOf(RSKeyValuePair)
          )
        }, logical(1)
      )
      if (!all(contamination)) {
        stop("InvalidArgumentError. 'key_value_pairs' are contaminated!")
      }
      for (entry in key_value_pairs) private$.hash_table$setObject(entry)
      return(invisible(self))
    },



    #' @description
    #' Sets a value for the given key.  
    #' The key is copied using `copyWithEnv()` and stored with the wrapped value in a key-value pair.
    #' If the key already exists, the value is replaced.
    #'
    #' @param value An `RSObject` to store.
    #' @param key An `RSObject` that serves as the key.
    setObjectForKey = function(value, key) {
      if (is.null(key$copyWithEnv)) stop("Key must adopt RSCopying Protocol.")
      key   <- key$copyWithEnv(environment(self))
      entry <- RSKeyValuePair$new(
        private$.wrapNull(key), private$.wrapNull(value)
      )
      private$.hash_table$setObject(entry)
    },



    #' @description
    #' Returns the value associated with the specified key, or `NULL` if not found.
    #' The key is normalized via `.wrapNull()`.
    #'
    #' @param key An `RSObject` key.
    #' @return The corresponding value (`RSObject`), or `NULL`.
    objectForKey = function(key) {
      found <- private$.getEntryByKey(private$.wrapNull(key))
      if (is.null(found)) return(NULL)
      found$value()
    },



    #' @description
    #' Removes the key-value pair associated with the given key, if present.
    #'
    #' @param key An `RSObject` to be removed.
    removeObjectForKey = function(key) {
      found <- private$.getEntryByKey(private$.wrapNull(key))
      if (!is.null(found)) private$.hash_table$removeObject(found)
    },



    #' @description
    #' Clears all key-value pairs from the dictionary.
    removeAllObjects = function() {
      private$.hash_table <- RSDynamicHash$new()
    },



    #' @description
    #' Returns a list of all `RSKeyValuePair` entries stored in the dictionary.
    allEntriesAsList = function() private$.hash_table$allObjectsAsList(),



    #' @description
    #' Returns the entries as an `RSArray` object.
    allEntries       = function() RSArray$new(self$allEntriesAsList()),



    #' @description
    #' Returns a list of all keys in the dictionary.
    allKeysAsList = function() {
      lapply(self$allEntriesAsList(), function(e) e$key())
    },



    #' @description
    #' Returns the keys as an `RSArray`.
    allKeys = function() RSArray$new(self$allKeysAsList()),



    #' @description
    #' Returns a list of all values in the dictionary.
    allValuesAsList  = function() {
      lapply(self$allEntriesAsList(), function(e) e$value())
    },



    #' @description
    #' Returns the values as an `RSArray`.
    allValues        = function() RSArray$new(self$allValuesAsList()),



    #' @description
    #' Alias for `allEntriesAsList()` / `allEntries()`.
    allObjectsAsList = function() self$allEntriesAsList(),



    #' @description
    #' Alias for `allEntriesAsList()` / `allEntries()`.
    allObjects       = function() self$allEntries(),



    #' @description
    #' Returns the number of key-value pairs in the dictionary.
    count = function() {
      private$.hash_table$count()
    },



    #' @description
    #' Returns an immutable `RSDictionary` copy of the current dictionary.
    #' All entries are preserved as-is and placed into the new environment.
    #' 
    #' @param env An environment for the copy to be evaluated in.
    copyWithEnv = function(env = parent.frame()){
      do.call(
        RSDictionary$new,
        list(key_value_pairs = self$allEntriesAsList()),
        envir = env
      )
    },



    #' @description
    #' Returns a new mutable copy of the dictionary.
    #' If the target environment matches the current one, the instance is returned unchanged.
    #' 
    #' @param env An environment for the copy to be evaluated in.
    mutableCopyWithEnv = function(env = parent.frame()) {
      if (identical(env, environment(self))) return(self)
      do.call(
        RSMutableDictionary$new,
        list(key_value_pairs = self$allEntriesAsList()),
        envir = env
      )
    },



    #' @description
    #' Returns a concise textual representation of the mutable dictionary object.
    #' The output includes the class name, a hexadecimal pointer string identifying the instance,
    #' and the numbers of entries.
    #' 
    #' @param ... ignored.
    description = function(...) {
      paste(
        super$description(),
        sprintf("%d entries", self$count())
      )
    },



    #' @description
    #' Returns a merged immutable dictionary containing all entries from this and another dictionary.
    #' Conflicting keys will prefer entries from `other`.
    #'
    #' @param other Another `RSMutableDictionary`.
    #' @return An `RSDictionary` instance.
    dictionaryByMergingWithDictionary  = function(other) {
      stopifnot(other$isKindOf(RSMutableDictionary))
      helper <- private$.hash_table$hash_operator
      merged <- helper$unionOfSets(
        self$allEntriesAsList(),
        other$allEntriesAsList()
      )
      return(RSDictionary$new(merged))
    },



    #' @description
    #' Returns a new immutable dictionary excluding keys found in `other`.
    #'
    #' @param other Another `RSMutableDictionary`.
    #' @return An `RSDictionary` instance.
    dictionaryByExcludingWithDictionary = function(other) {
      stopifnot(other$isKindOf(RSMutableDictionary))
      helper <- private$.hash_table$hash_operator
      excluded <- helper$differenceOfSets(
        self$allEntriesAsList(),
        other$allEntriesAsList()
      )
      return(RSDictionary$new(excluded))
    }
  )
)
