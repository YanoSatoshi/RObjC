#' @include RSMutableDictionary.r
#' @title RSDictionary: Native-Backed Immutable Key-Value Store
#'
#' @description
#' `RSDictionary` is the immutable counterpart of [RSMutableDictionary]. It uses
#' the same native-backed storage and lookup behavior, while rejecting every
#' public mutation operation.
#'
#' `copy()` returns the receiver. `mutableCopy()` creates independent native
#' storage that initially references the same key and value objects.
#'
#' @section Convenience Constructor:
#' [RSDictionary_withDictionary()] creates an immutable shallow copy from a
#' mutable dictionary. An immutable dictionary is returned unchanged.
#'
#' @export
RSDictionary <- RSObjectClass(
  "RSDictionary",
  version = "0.1.0 beta",
  super_class = RSMutableDictionary,
  protocols = list(RSCopying, RSMutableCopying),
  public = list(
    #' @description
    #' Constructs an immutable dictionary from one supported input format.
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
      super$initialize(
        key_value_pairs = key_value_pairs,
        objects_and_keys = objects_and_keys,
        keys = keys,
        values = values
      )
      invisible(self)
    },

    #' @description Disabled because the dictionary is immutable.
    #' @param value Ignored.
    #' @param key Ignored.
    setObjectForKey = function(value, key) {
      stop(msg_immutable_try_mutate(self$class()))
    },

    #' @description Disabled because the dictionary is immutable.
    #' @param key Ignored.
    removeObjectForKey = function(key) {
      stop(msg_immutable_try_mutate(self$class()))
    },

    #' @description Disabled because the dictionary is immutable.
    removeAllObjects = function() {
      stop(msg_immutable_try_mutate(self$class()))
    },

    #' @description Disabled because the dictionary is immutable.
    #' @param entry Ignored.
    addEntry = function(entry) {
      stop(msg_immutable_try_mutate(self$class()))
    },

    #' @description Disabled because the dictionary is immutable.
    #' @param entry Ignored.
    removeEntry = function(entry) {
      stop(msg_immutable_try_mutate(self$class()))
    },

    #' @description Disabled because the dictionary is immutable.
    removeAllEntries = function() {
      stop(msg_immutable_try_mutate(self$class()))
    },

    #' @description Returns the receiver because it is immutable.
    #' @return `self`.
    copy = function() self,

    #' @description Returns an independent mutable shallow copy.
    #' @return A new [RSMutableDictionary].
    mutableCopy = function() {
      super$mutableCopy()
    }
  )
)
