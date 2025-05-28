#' @title RSDictionary: Immutable Key-Value Store
#'
#' @description
#' `RSDictionary` is an immutable subclass of `RSMutableDictionary`. Its structure is frozen
#' after initialization, and all mutation methods raise errors. It provides efficient access
#' via `objectForKey()`, and supports deep copying into mutable or immutable versions.
#'
#' @note Attempting to use `setObjectForKey()`, `removeObjectForKey()`, or `removeAllObjects()`
#' will raise an error.
#'
#' @export
RSDictionary <- R6::R6Class(
  "RSDictionary",
  inherit = RSMutableDictionary,
  cloneable = FALSE,
  private = list(
    .flat = NULL,
    .freeze = function() {
      private$.flat <- private$.hash_table$allObjectsAsList()
      private$.hash_table <- NULL
    }
  ),
  public = list(

    #' @description
    #' Constructs an immutable dictionary using one of three formats: a list of key-value pairs,
    #' an alternating key-value list, or separate key and value lists.
    #' Keys and values are wrapped and stored in the internal hash table.
    #'
    #' @param key_value_pairs A list of `RSKeyValuePair` objects.
    #' @param objects_and_keys Alternating list of \code{list(key0, value0, key1, value1, ...)}.
    #' @param keys List or vector of key objects.
    #' @param values List or vector of value objects.
    initialize = function(
      key_value_pairs  = NULL, # default:          list[RSKeyValuePair] # nolint
      objects_and_keys = NULL, # alternative:      list[key0, value0, key1, value1, ...] # nolint
      keys   = NULL,           # give with values: list[key0, key1, ...]
      values = NULL            # give with keys:   list[value0, value1, ...]
    ) {
      super$initialize(
        key_value_pairs = key_value_pairs,
        objects_and_keys = objects_and_keys,
        keys = keys,
        values = values
      )
      private$.freeze()
      invisible(self)
    },



    #' @description
    #' This method is disabled for immutable `RSDictionary` objects.
    #' Attempting to call this method will result in an error.
    #' @param value Ignored.
    #' @param key Ignored.
    #' @return No return value. Always raises an error.
    setObjectForKey = function(value, key) {
      stop(msg_immutable_try_mutate(self$class()))
    },



    #' @description
    #' This method is disabled for immutable `RSDictionary` objects.
    #' Attempting to call this method will result in an error.
    #' @param key Ignored.
    #' @return No return value. Always raises an error.
    removeObjectForKey = function(key) {
      stop(msg_immutable_try_mutate(self$class()))
    },



    #' @description
    #' This method is disabled for immutable `RSDictionary` objects.
    #' Attempting to call this method will result in an error.
    #' @return No return value. Always raises an error.
    removeAllObjects = function() {
      stop(msg_immutable_try_mutate(self$class()))
    },



    #' @description
    #' Returns the value associated with the specified key, or `NULL` if not found.
    #' The key is normalized via `.wrapNull()`.
    #'
    #' @param key An `RSObject` key.
    #' @return The corresponding value (`RSObject`), or `NULL`.
    objectForKey = function(key) {
      if (is.null(key$isKindOf) || !key$isKindOf(RSObject)) {
        stop("InvalidArgumentError. Key must be a kind of RSObject class")
      }
      hash_val <- key$hash()
      match <- Find(
        function(entry) {
          entry$hash() == hash_val && key$isEqual(entry$key())
        },
        private$.flat
      )
      if (is.null(match)) return(NULL)
      match$value()
    },



    #' @description
    #' Returns a list of all `RSKeyValuePair` entries stored in the dictionary.
    allEntriesAsList = function() private$.flat,



    #' @description
    #' Returns the number of key-value pairs in the dictionary.
    count = function() {
      length(private$.flat)
    },



    #' @description
    #' Returns the object itself, since `RSDictionary` is immutable.
    #'
    #' This is a no-op copy: the method returns `self`, as immutable
    #' dictionaries are safe to reuse without duplication.
    #'
    #' @return The same `RSDictionary` instance (`self`).
    copy = function() return(self),



    #' @description
    #' Returns the object itself, since `RSDictionary` is immutable.
    #'
    #' This is a no-op copy: the method returns `self`, as immutable
    #' dictionaries are safe to reuse without duplication.
    #'
    #' @return The same `RSDictionary` instance (`self`).
    mutableCopy = function() RSMutableDictionary$new(key_value_pairs = self$allEntriesAsList())
  )
)
