#' @include api_setup.r
#' @include RSScalar.r
#' @title RSNull: Singleton Wrapper Representing R's NULL
#'
#' @description
#' `RSNull` is a singleton class derived from `RSScalar` that represents the `NULL` value in R.
#' It provides a uniform object-based null placeholder, similar to `NSNull` in Objective-C,
#' and is designed to be used where scalar-wrapping behavior is required even for `NULL`.
#'
#' [RSScalarClass()] creates the shared instance when `RSNull` is declared.
#' Access it through `RSNull$null` (an alias of `RSNull$instance`).
#'
#' @section Methods:
#' - `value()` – Always returns `NULL`
#' - `hash()` – Always returns `0L`
#' - `isEqual(other)` – Returns TRUE if `other` is of the same class
#' - `lockVariables()` – Prevents structural modification
#' 
#' @section Usage:
#' Use `RSNull$null` to retrieve the shared instance. The current factory does
#' not disable `$new()`, so constructing another instance is possible.
#'
#' @note
#' The `RSNull$instance` and `RSNull$null` bindings are locked after creation.
#'
#' @examples
#' x <- RSNull$null
#' x$value()      # returns NULL
#' x$isEqual(RSNull$null)  # TRUE
#'
#' @export
RSNull <- RSScalarClass(
  "RSNull",
  super_class = RSScalar,
  is_na_class = NA,
  singleton = TRUE,
  public = list(
    #' @description
    #' Initializes an `RSNull` instance. The factory invokes it to create the
    #' shared instance; use `RSNull$null` to retrieve that instance.
    #'
    #' It delegates scalar initialization, then stores `NULL` as the value.
    #'
    #' @return The initialized `RSNull` instance (invisible).
    initialize = function() {
      super$initialize(0L)
      private$.val <- NULL
      invisible(self)
    },



    #' @description
    #' Determines whether another object is also an instance of the `RSNull` class.
    #' This method checks for membership in the same class rather than strict identity.
    #'
    #' @param other Another object to compare against.
    #' @return `TRUE` if `other` is also an `RSNull` instance; otherwise `FALSE`.
    isEqual = function(other) {
      self$isMemberOf(other$class())
    },



    #' @description
    #' Returns a fixed integer hash value (`0L`) for all `RSNull` instances.
    #' Since `RSNull` is a singleton and always represents the same conceptual null value,
    #' a constant hash ensures consistent behavior in hashed collections.
    #'
    #' @return Integer scalar `0L`.
    hash  = function() 0L,



    #' @description
    #' Returns the internal null value (`NULL`) wrapped by the `RSNull` instance.
    #'
    #' @return `NULL`.
    value = function() NULL,


    #' @description
    #' Locks all current public bindings in the `RSNull` instance, making them read-only.
    #' This prevents accidental mutation of the singleton after creation.
    #'
    #' @return Called for side effect. Returns `NULL` (invisible).
    lockVariables = function() {
      for (name in ls(self, all.names = TRUE)) lockBinding(name, self)
      lockEnvironment(as.environment(self), bindings = TRUE)
    }
  )
)
RSNull$null <- RSNull$instance
lockBinding("null", RSNull)
