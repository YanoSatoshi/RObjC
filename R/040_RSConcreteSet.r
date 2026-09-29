#' @title Native-Backed Set Storage
#'
#' @description
#' `RSConcreteSet` is the internal storage adapter shared by [RSSet] and
#' [RSMutableSet]. It lazily owns an external pointer to a C++ `NativeSet` and
#' keeps all Rcpp bridge calls out of the public set classes.
#'
#' This class provides storage semantics only. It does not inherit from
#' [RSObject], copy inserted objects, translate `NULL`, or impose mutable versus
#' immutable behavior. Objects are retained by the native storage and equality
#' is determined by their `hash()` and `isEqual()` methods.
#'
#' The set operations mutate the receiver. They correspond to the operations
#' provided by Foundation's mutable set: union, intersection, and subtraction.
#'
#' @section Methods:
#' - `duplicate()` returns an independent shallow copy of the native table.
#' - `add(item)` inserts an item when no equal member exists.
#' - `member(item)` returns the stored equal member, or `NULL` when absent.
#' - `contains(item)` reports whether an equal member exists.
#' - `remove(item)` removes an equal member and reports whether it existed.
#' - `clear()` removes every member.
#' - `allObjects()` returns the stored objects in unspecified order.
#' - `count()` returns the number of members.
#' - `uniteWith(other)`, `intersectWith(other)`, and `subtractWith(other)` mutate
#'   this storage using another `RSConcreteSet`.
#' - `contentHash()` returns an order-independent aggregate member hash.
#' - `prepareForSerialization()` captures a temporary R snapshot used only to
#'   rebuild the native table after deserialization.
#'
#' @keywords internal
#' @return An `RSConcreteSet` instance.
RSConcreteSet <- R6::R6Class(
  "RSConcreteSet",
  cloneable = FALSE,
  private = list(
    .native_ptr = NULL,
    .serialized_objects = NULL,
    .serialization_prepared = FALSE,

    .native = function() {
      if (is.null(private$.native_ptr)) {
        private$.native_ptr <- set_create()
      }
      if (private$.serialization_prepared) {
        valid <- tryCatch(
          {
            set_size(private$.native_ptr)
            TRUE
          },
          error = function(condition) FALSE
        )
        if (!valid) {
          private$.native_ptr <- set_create()
          for (item in private$.serialized_objects) {
            set_add(private$.native_ptr, item)
          }
        }
        private$.serialized_objects <- NULL
        private$.serialization_prepared <- FALSE
      }
      private$.native_ptr
    },

    .nativeFrom = function(other) {
      if (!inherits(other, "RSConcreteSet")) {
        stop("other must be an RSConcreteSet")
      }
      other$.__enclos_env__$private$.native()
    }
  ),
  public = list(
    #' @description Creates empty native storage or adopts an internal pointer.
    #' @param .native_ptr Internal external pointer returned by the native set
    #'   bridge. Package code should normally leave this as `NULL`.
    initialize = function(.native_ptr = NULL) {
      if (!is.null(.native_ptr)) {
        if (typeof(.native_ptr) != "externalptr") {
          stop(".native_ptr must be an external pointer")
        }
        private$.native_ptr <- .native_ptr
      }
      invisible(self)
    },

    #' @description Creates an independent shallow copy of the storage.
    #' @return A new `RSConcreteSet` containing the same object references.
    duplicate = function() {
      RSConcreteSet$new(
        .native_ptr = set_clone(private$.native())
      )
    },

    #' @description Adds an object if no equal member is present.
    #' @param item A hashable RObjC object. The object is retained, not copied.
    #' @return `self`, invisibly.
    add = function(item) {
      set_add(private$.native(), item)
      invisible(self)
    },

    #' @description Returns the stored object equal to `item`.
    #' @param item A hashable RObjC object used for lookup.
    #' @return The stored member, or `NULL` when absent.
    member = function(item) {
      set_member(private$.native(), item)
    },

    #' @description Returns an arbitrary stored member.
    #' @return A stored member, or `NULL` when the set is empty.
    anyObject = function() {
      set_any_object(private$.native())
    },

    #' @description Tests whether an equal member is present.
    #' @param item A hashable RObjC object used for lookup.
    #' @return A logical scalar.
    contains = function(item) {
      set_contains(private$.native(), item)
    },

    #' @description Removes the member equal to `item`.
    #' @param item A hashable RObjC object used for lookup.
    #' @return `TRUE` if a member was removed; otherwise `FALSE`.
    remove = function(item) {
      set_remove(private$.native(), item)
    },

    #' @description Removes every member.
    #' @return `self`, invisibly.
    clear = function() {
      set_clear(private$.native())
      invisible(self)
    },

    #' @description Returns all stored members.
    #' @return A list in unspecified order.
    allObjects = function() {
      set_values(private$.native())
    },

    #' @description Returns the number of members.
    #' @return A non-negative numeric scalar.
    count = function() {
      set_size(private$.native())
    },

    #' @description Adds every member of `other` to this storage.
    #' @param other Another `RSConcreteSet`.
    #' @return `self`, invisibly.
    uniteWith = function(other) {
      set_unite_with(private$.native(), private$.nativeFrom(other))
      invisible(self)
    },

    #' @description Retains only members also present in `other`.
    #' @param other Another `RSConcreteSet`.
    #' @return `self`, invisibly.
    intersectWith = function(other) {
      set_intersect_with(private$.native(), private$.nativeFrom(other))
      invisible(self)
    },

    #' @description Removes every member also present in `other`.
    #' @param other Another `RSConcreteSet`.
    #' @return `self`, invisibly.
    subtractWith = function(other) {
      set_subtract_with(private$.native(), private$.nativeFrom(other))
      invisible(self)
    },


    #' @description Computes an order-independent aggregate member hash.
    #' @return An integer hash value.
    contentHash = function() {
      RSHashOperator$module()$hashFromObjects(self$allObjects())
    },

    #' @description Captures data needed to rebuild storage after serialization.
    #' @return `self`, invisibly.
    #' @keywords internal
    prepareForSerialization = function() {
      private$.serialized_objects <- self$allObjects()
      private$.serialization_prepared <- TRUE
      invisible(self)
    }
  )
)
