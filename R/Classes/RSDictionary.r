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
    setObjectForKey = function(value, key) {
      stop(msg_immutable_try_mutate(self$class()))
    },
    removeObjectForKey = function(key) {
      stop(msg_immutable_try_mutate(self$class()))
    },
    removeAllObjects = function() {
      stop(msg_immutable_try_mutate(self$class()))
    },
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
    allEntriesAsList = function() private$.flat,
    count = function() {
      length(private$.flat)
    },



#' @describeIn RSDictionary
#' Returns an identical immutable copy of the current object placed into the specified environment.
#' Because the object is already immutable, this method typically returns `self` unless an environment
#' switch is needed for structural consistency.
#'
#' @param env The target environment.
#' @return A copy of the current immutable object, typically `self`.
    copyWithEnv = function() {
      if (identical(env, environment(self))) return(self)
      do.call(
        RSDictionaryy$new,
        list(key_value_pairs = self$allEntriesAsList())
      )
    },



#' @describeIn RSDictionary
#' Returns a new mutable version of the current object in the specified environment.
#' The content is duplicated to allow modifications, and the result is typically
#' an instance of the corresponding mutable subclass (e.g., `RSMutableArray` for `RSArray`).
#'
#' @param env The environment in which the new mutable object will be evaluated.
#' @return A new mutable copy of the object.
    mutableCopyWithEnv = function() {
      do.call(
        RSMutableDictionary$new,
        list(key_value_pairs = self$allEntriesAsList())
      )
    }
  )
)
