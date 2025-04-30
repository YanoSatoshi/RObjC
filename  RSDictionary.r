#' RSDictionary
RSDictionary <- R6::R6Class(
  "RSDictionary",
  inherit = RSMutableDictionary,
  cloneable = FALSE,
  public = list(
    initialize = function(
      key_value_pairs  = NULL, # default:          list[RSKeyValuePair]
      objects_and_keys = NULL, # alternative:      list[key0, value0, key1, value1, ...]
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
      stop("RSDictionary is immutable. Use RSMutableDictionary for modification.")
    },
    removeObjectForKey = function(key) {
      stop("RSDictionary is immutable. Use RSMutableDictionary for modification.")
    },
    removeAllObjects = function() {
      stop("RSDictionary is immutable. Use RSMutableDictionary for modification.")
    },
    allKeys = function() {
      lapply(private$.flat, function(pair) pair$key())
    },
    allValues = function() {
      lapply(private$.flat, function(pair) pair$value())
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
    count = function() {
      length(private$.flat)
    }
  ),
  private = list(
    .flat = NULL,
    .freeze = function() {
      private$.flat <- private$.hash_table$allObjects()
      private$.hash_table <- NULL
    }
  )
)
