#' RSMutableDictionary
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
    setObjectForKey = function(value, key) {
      if (is.null(key$copyWithEnv)) stop("Key must adopt RSCopying Protocol.")
      key   <- key$copyWithEnv(environment(self))
      entry <- RSKeyValuePair$new(
        private$.wrapNull(key), private$.wrapNull(value)
      )
      private$.hash_table$setObject(entry)
    },
    objectForKey = function(key) {
      found <- private$.getEntryByKey(private$.wrapNull(key))
      if (is.null(found)) return(NULL)
      found$value()
    },
    removeObjectForKey = function(key) {
      found <- private$.getEntryByKey(private$.wrapNull(key))
      if (!is.null(found)) private$.hash_table$removeObject(found)
    },
    allEntriesAsList = function() private$.hash_table$allObjectsAsList(),
    allEntries       = function() RSArray$new(self$allEntriesAsList()),
    allKeysAsList = function() {
      lapply(self$allEntriesAsList(), function(e) e$key())
    },
    allKeys = function() RSArray$new(self$allKeysAsList()),
    allValuesAsList  = function() {
      lapply(self$allEntriesAsList(), function(e) e$value())
    },
    allValues        = function() RSArray$new(self$allValuesAsList()),
    allObjectsAsList = function() self$allEntriesAsList(),
    allObjects       = function() self$allEntries(),
    count = function() {
      private$.hash_table$count()
    },
    removeAllObjects = function() {
      private$.hash_table <- RSDynamicHash$new()
    },
    copyWithEnv = function(env = parent.frame()){
      do.call(
        RSDictionary$new,
        list(key_value_pairs = self$allEntriesAsList()),
        envir = env
      )
    },
    mutableCopyWithEnv = function(env = parent.frame()) {
      if (identical(env, environment(self))) return(self)
      do.call(
        RSMutableDictionary$new,
        list(key_value_pairs = self$allEntriesAsList()),
        envir = env
      )
    },
    description = function(...) {
      paste(
        super$description(),
        sprintf("%d entries", self$count())
      )
    },
    dictionaryByMergingWithDictionary  = function(other) {
      stopifnot(other$isKindOf(RSMutableDictionary))
      helper <- private$.hash_table$hash_operator
      merged <- helper$unionOfSets(
        self$allEntriesAsList(),
        other$allEntriesAsList()
      )
      return(RSDictionary$new(merged))
    },
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
