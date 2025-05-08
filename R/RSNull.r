#' @title RSNull: Singleton Wrapper Representing R's NULL
#'
#' @description
#' `RSNull` is a singleton class derived from `RSScalar` that represents the `NULL` value in R.
#' It provides a uniform object-based null placeholder, similar to `NSNull` in Objective-C,
#' and is designed to be used where scalar-wrapping behavior is required even for `NULL`.
#'
#' Instantiation is restricted to a single shared instance accessed via `RSNull$null()`.
#' Attempting to call `RSNull$new()` directly will raise an error.
#'
#' @section Methods:
#' - `value()` – Always returns `NULL`
#' - `hash()` – Always returns `0L`
#' - `isEqual(other)` – Returns TRUE if `other` is of the same class
#' - `lockVariables()` – Prevents structural modification
#' 
#' @section Usage:
#' Use \code{RSNull$null()} to retrieve the singleton instance. Do not call \code{RSNull$new()} directly.
#'
#' @note
#' The singleton instance is assigned to `RSNull$instance` and locked after initialization.
#'
#' @examples
#' x <- RSNull$null()
#' x$value()      # returns NULL
#' x$isEqual(RSNull$null())  # TRUE
#'
#' @export
RSNull <- R6::R6Class(
  "RSNull",
  cloneable = FALSE,
  inherit = RSScalar,
  public = list(

    #' @description
    #' Initializes the singleton instance of `RSNull`. This constructor should not be called
    #' directly. Instead, use `RSNull$null()` to retrieve the unique instance.
    #' If an instance already exists, this method raises an error.
    #'
    #' Internally, it sets `.val` to `NULL`, registers the singleton instance,
    #' and locks the object to prevent further modification.
    #'
    #' @return The initialized `RSNull` instance (invisible).
    initialize = function() {
      if (!is.null(RSNull$.singleton)) {
        stop("Use RSNull$null() instead of new().")
      }
      super$initialize(0L)
      private$.val <- NULL
      klass <- self$class()
      klass$null <- function() klass$.singleton
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
RSNull$isNAClass <- NA
register_rs_singleton(RSNull, RSNull$new())


# # @title Return the Singleton Instance of RSNull
# #
# # @name null
# # 
# # @description
# # Returns the shared singleton instance of `RSNull`. If not yet created,
# # it is initialized and stored in `RSNull$instance`.
# #
# # @return An instance of `RSNull`.
# # @export
# RSNull$null <- function() {
#   if (is.null(RSNull$instance)) return(RSNull$new())
#   RSNull$instance
# }
