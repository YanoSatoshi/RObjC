#' RSMutableDictionary
RSMutableDictionary <- R6::R6Class(
  "RSMutableDictionary",
  cloneable = FALSE,
  inherit = RSObject,
  public = list(
    initialize = function(
      key_value_pairs  = NULL, # default:          list[RSKeyValuePair]
      objects_and_keys = NULL, # alternative:      list[key0, value0, key1, value1, ...]
      keys   = NULL,           # give with values: list[key0, key1, ...]
      values = NULL            # give with keys:   list[value0, value1, ...]
    ) {
      is_kv_pair  <- !is.null(key_value_pairs)
      is_alt_list <- !is.null(objects_and_keys)
      is_separate <- !is.null(keys) && !is.null(values)
      if (sum(is_kv_pair, is_alt_list, is_separate) > 1) {
        stop("InvalidArgumentError. Choose a initializing by key_value_pairs or objects_and_keys, or 'keys and values'")
      }
      if (sum(is_kv_pair, is_alt_list, is_separate) == 0) {
        # returns empty dict
        super$initialize()
        private$.hash_table <- RSDynamicHash$new()
        return(invisible(self))
      }
      if (!is_kv_pair && is_alt_list) {
        if (length(objects_and_keys) %% 2 != 0) {
          stop("InvalidArgumentError. length of 'objects_and_keys' must be even.")
        }
        keys   <- objects_and_keys[seq(1, length(objects_and_keys), by = 2)]
        values <- objects_and_keys[seq(2, length(objects_and_keys), by = 2)]
        is_separate <- TRUE
      }
      if (!is_kv_pair && is_separate) {
        # lengthCheck
        if (length(keys) != length(values)) {
          stop("InvalidArgumentError. 'keys' and 'values' must have the same length.")
        }
        key_value_pairs <- mapply(
          function(aKey, aValue) {
            RSKeyValuePair$new(private$.wrapNull(aKey), private$.wrapNull(aValue))
          },
          keys, values, SIMPLIFY = FALSE
        )
      }
      # finally
      if(!is.null(key_value_pairs)) {
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
        super$initialize()
        private$.hash_table <- RSDynamicHash$new()
        for (entry in key_value_pairs) private$.hash_table$setObject(entry)
        return(invisible(self))
      }
      return(NULL)
    },
    setObjectForKey = function(value, key) {
      wrapped_key   <- private$.wrapNull(key)
      wrapped_value <- private$.wrapNull(value)
      entry <- RSKeyValuePair$new(wrapped_key, wrapped_value)
      private$.hash_table$setObject(entry)
    },
    objectForKey = function(key) {
      if (is.null(key)) key <- RSNull$null()
      found <- private$.getEntryByHash(key)
      if (is.null(found)) return(NULL)
      found$value()
    },
    removeObjectForKey = function(key) {
      if (is.null(key)) key <- RSNull$null()
      found <- private$.getEntryByHash(key)
      if (!is.null(found)) {
        private$.hash_table$removeObject(found)
      }
    },
    allKeys = function() {
      entries <- private$.hash_table$allObjects()
      if (length(entries) == 0) return(list())
      lapply(entries, function(entry) entry$key())
    },
    allValues = function() {
      entries <- private$.hash_table$allObjects()
      if (length(entries) == 0) return(list())
      lapply(entries, function(entry) entry$value())
    },
    allEntries = function() {
      private$.hash_table$allObjects()
    },
    count = function() {
      private$.hash_table$count()
    },
    removeAllObjects = function() {
      private$.hash_table <- RSDynamicHash$new()
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
      merged <- helper$unionOfSets(self$allEntries(), other$allEntries())
      return(RSDictionary$new(merged))
    },
    dictionaryByExcludingWithDictionary = function(other) {
      stopifnot(other$isKindOf(RSMutableDictionary))
      helper <- private$.hash_table$hash_operator
      excluded <- helper$differenceOfSets(self$allEntries(), other$allEntries())
      return(RSDictionary$new(excluded))
    }
  ),
  private = list(
    .hash_table = NULL,
    .check_atomic = function(some) {
      !any(vapply(some, function(x) is.atomic(some), logical(1)))
    },
    .wrapNull = function(some) {
      if (is.null(some)) return(RSNull$null())
      if (!is.null(some$isMemberOf) && some$isKindOf(RSObject)) return(some)
      stop(sprintf("Unsupported key type: %s", class(some)[1]))
    },
    .getEntryByHash = function(entity_key) {
      same_hash_list <- private$.hash_table$getObjectsByHash(entity_key$hash())
      Find(
        nomatch = NULL,
        function(entry) {entity_key$isEqual(entry$key())},
        same_hash_list
      )
    }
  )
)
