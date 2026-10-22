#' @title Native-Backed Dictionary Storage
#'
#' @description
#' `RSConcreteDictionary` is the internal storage adapter shared by
#' [RSDictionary] and [RSMutableDictionary]. It lazily owns an external pointer
#' to a C++ `NativeDictionary` and keeps all Rcpp bridge calls out of the public
#' dictionary classes.
#' The native table calls each key's `hash()` and `isEqual()` methods for
#' lookup; keys must keep their hash and equality state stable while stored.
#'
#' This class provides storage semantics only. It does not inherit from
#' [RSObject], copy keys, translate `NULL` to [RSNull], or impose mutable versus
#' immutable behavior. Those responsibilities belong to the public dictionary
#' wrappers.
#'
#' @section Methods:
#' - `duplicate()` returns an independent shallow copy of the native storage.
#' - `set(key, value)` inserts or replaces a value.
#' - `get(key)` returns the value for a key, or `NULL` when absent.
#' - `contains(key)` reports whether a key is present.
#' - `remove(key)` removes a key and reports whether it existed.
#' - `clear()` removes every entry.
#' - `keys()` and `values()` return the stored objects as lists.
#' - `entries()` returns corresponding key-value pairs as two-element lists.
#' - `count()` returns the number of entries.
#' - `contentHash()` returns an order-independent aggregate hash of the keys.
#' - `prepareForSerialization()` captures a temporary R snapshot used only to
#'   rebuild the native table after deserialization.
#'
#' @keywords internal
#' @return An `RSConcreteDictionary` instance.
RSConcreteDictionary <- R6::R6Class(
  "RSConcreteDictionary",
  class     = TRUE,
  cloneable = FALSE,
  portable  = TRUE,
  active    = NULL,
  inherit   = NULL,
  private = list(
    .native_ptr = NULL,
    .serialized_keys = NULL,
    .serialized_values = NULL,
    .serialization_prepared = FALSE,
    .native = function() {
      if (is.null(private$.native_ptr)) {
        private$.native_ptr <- dictionary_create()
      }
      if (private$.serialization_prepared) {
        valid <- tryCatch(
          {
            dictionary_size(private$.native_ptr)
            TRUE
          },
          error = function(condition) FALSE
        )
        if (!valid) {
          private$.native_ptr <- dictionary_create()
          for (i in seq_along(private$.serialized_keys)) {
            dictionary_set(
              private$.native_ptr,
              private$.serialized_keys[[i]],
              private$.serialized_values[[i]]
            )
          }
        }
        private$.serialized_keys <- NULL
        private$.serialized_values <- NULL
        private$.serialization_prepared <- FALSE
      }
      private$.native_ptr
    }
  ),
  public = list(
    #' @description Creates empty native storage or adopts an internal pointer.
    #' @param .native_ptr Internal external pointer returned by the native
    #'   dictionary bridge. Package code should normally leave this as `NULL`.
    initialize = function(.native_ptr = NULL) {
      if (!is.null(.native_ptr)) {
        if (typeof(.native_ptr) != "externalptr")
          stop(".native_ptr must be an external pointer")
        private$.native_ptr <- .native_ptr
      }
      invisible(self)
    },

    #' @description Creates an independent shallow copy of the storage.
    #' @return A new `RSConcreteDictionary` instance.
    duplicate = function() {
      RSConcreteDictionary$new(
        .native_ptr = dictionary_clone(private$.native())
      )
    },

    #' @description Inserts or replaces a value for a key.
#' @param key Hashable RObjC object or Class/Protocol generator key.
    #' @param value Object to store.
    #' @return `self`, invisibly.
    set = function(key, value) {
      dictionary_set(private$.native(), key, value)
      invisible(self)
    },

    #' @description Returns the value associated with a key.
    #' @param key Hashable RObjC object or Class/Protocol generator key.
    #' @return The stored value, or `NULL` when the key is absent.
    get = function(key) {
      dictionary_get(private$.native(), key)
    },

    #' @description Tests whether the storage contains a key.
    #' @param key Hashable RObjC object or Class/Protocol generator key.
    #' @return A logical scalar.
    contains = function(key) {
      dictionary_contains(private$.native(), key)
    },

    #' @description Removes the entry associated with a key.
    #' @param key Hashable RObjC object or Class/Protocol generator key.
    #' @return `TRUE` if an entry was removed; otherwise `FALSE`.
    remove = function(key) {
      dictionary_remove(private$.native(), key)
    },

    #' @description Removes every entry.
    #' @return `self`, invisibly.
    clear = function() {
      dictionary_clear(private$.native())
      invisible(self)
    },

    #' @description Returns all stored keys.
    #' @return A list of keys in unspecified order.
    keys = function() {
      dictionary_keys(private$.native())
    },

    #' @description Returns all stored values.
    #' @return A list of values corresponding to `keys()` when unmodified.
    values = function() {
      dictionary_values(private$.native())
    },

    #' @description Returns corresponding stored keys and values together.
    #' @return A list of two-element lists, each containing a key and its value.
    entries = function() {
      dictionary_entries(private$.native())
    },

    #' @description Returns the number of entries.
    #' @return A non-negative numeric scalar.
    count = function() {
      dictionary_size(private$.native())
    },

    #' @description Computes an order-independent aggregate hash of the keys.
    #' @return An integer hash value.
    contentHash = function() {
      RSHashOperator$instance$hashFromObjects(self$keys())
    },

    #' @description Captures data needed to rebuild storage after serialization.
    #' @return `self`, invisibly.
    #' @keywords internal
    prepareForSerialization = function() {
      private$.serialized_keys <- self$keys()
      private$.serialized_values <- self$values()
      private$.serialization_prepared <- TRUE
      invisible(self)
    }
  )
)
