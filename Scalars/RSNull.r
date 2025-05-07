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
    initialize = function() {
      if (!is.null(RSNull$instance)) {
        stop("Use RSNull$null() instead of new().")
      }
      super$initialize(0L)
      private$.val <- NULL
      RSNull$instance <<- self
      self$lockVariables()
      lockBinding("instance", RSNull)
      invisible(self)
    },
    isEqual = function(other) {
      self$isMemberOf(other$class())
    },
    hash  = function() 0L,
    value = function() NULL,
    # Locks all current bindings in the object to prevent accidental additions or removals.
    lockVariables = function() {
      for (name in ls(self, all.names = TRUE)) lockBinding(name, self)
      lockEnvironment(as.environment(self), bindings = TRUE)
    }
  )
)
RSNull$instance  <<- NULL
RSNull$isNAClass <- NA



#' @title Return the Singleton Instance of RSNull
#'
#' @description
#' Returns the shared singleton instance of `RSNull`. If not yet created,
#' it is initialized and stored in `RSNull$instance`.
#'
#' @return An instance of `RSNull`.
#' @export
RSNull$null <- function() {
  if (is.null(RSNull$instance)) return(RSNull$new())
  RSNull$instance
}
