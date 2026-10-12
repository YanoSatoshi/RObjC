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
RSNull <- RSSingletonClass(
  "RSNull",
  super_class = RSScalar,
  version     = "0.0.2 alpha",
  public      = list(
    #' @description
    #' Initializes an `RSNull` instance. The factory invokes it to create the
    #' shared instance; use `RSNull$null` to retrieve that instance.
    #'
    #' It delegates scalar initialization, then stores `NULL` as the value.
    #'
    #' @return The initialized `RSNull` instance (invisible).
    initialize = function() {
      if (!is.null(self$class()$null)) {
        class_neme <- self$class()$classname
        stop(paste0(
          "Use `",class_name,"$null`. Do not call `",class_name,"$new()`",
        ))
      }
      super$initialize(0L)
      private$.val <- NULL
      invisible(self)
    },



    #' @description
    #' Determines whether another object is identical to self: `RSNull` singleton.
    #'
    #' @param other Another object to compare against.
    #' @return `TRUE` if `other` is also an `RSNull` instance; otherwise `FALSE`.
    isEqual = function(other) identical(self, other),



    #' @description
    #' Returns a fixed integer hash value (`0L`) for all `RSNull` instances.
    #' Since `RSNull` is a singleton and always represents the same conceptual null value,
    #' a constant hash ensures consistent behavior in hashed collections.
    #'
    #' @return Integer scalar `0L`.
    hash    = function() 0L,



    #' @description
    #' Returns the internal null value (`NULL`) wrapped by the `RSNull` instance.
    #'
    #' @return `NULL`.
    value = function() NULL
  )
)
RSNull$null <- RSNull$instance
RSNull$is_na_class <- NA
lockBinding("null", RSNull)
lockBinding("is_na_class", RSNull)