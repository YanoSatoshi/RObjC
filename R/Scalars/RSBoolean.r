#' @title RSBoolean: Logical Scalar Wrapper
#'
#' @description
#' `RSBoolean` wraps a logical (TRUE/FALSE) scalar. It inherits from `RSScalar` and
#' provides boolean-specific behavior, including conversion to numeric via `asNumber()`.
#' It rejects NA values; use `RSNABoolean` for NA.
#'
#' @section Methods:
#' - `asNumber()` — Returns an `RSNumber` with value 1 or 0.
#'
#' @section Usage:
#' Use `RSBoolean$boolWith(val)` to construct safely.
#'
#' @export
RSBoolean <- R6::R6Class(
  "RSBoolean",
  cloneable = FALSE,
  inherit = RSScalar,
  public = list(
    initialize = function(val) {
      if (!inherits(val, "logical")) stop("val must be of class logical")
      super$initialize(val)
      invisible(self)
    },
    ## CAST TO RSNumber
    asNumber = function() {
      RSNumber$new(as.numeric(private$.val))
    }
  )
)
RSBoolean$isNAClass <- FALSE


#' @title Factory method for RSBoolean
#' @description
#' Wraps a logical scalar into an `RSBoolean` object. If the value is `NA`, returns an `RSNABoolean`.
#' @param val A logical value (length 1).
#' @return An `RSBoolean` or `RSNABoolean` object.
#' @export
RSBoolean$boolWith  <- function(val) {
  if (is.na(val)) return(RSNABoolean$new())
  return(RSBoolean$new(as.logical(val)))
}



#' @title RSNABoolean: Wrapper for NA-logical Scalar
#'
#' @description
#' `RSNABoolean` is a subclass of `RSBoolean` that safely represents `NA` logical values.
#' This class is typically used internally by factory methods.
#'
#' @note
#' Use `RSBoolean$boolWith(val)` instead of instantiating directly.
#'
#' @export
RSNABoolean <- R6::R6Class(
  "RSNABoolean",
  cloneable = FALSE,
  inherit = RSBoolean,
  public = list(
    initialize = function() {
      super$initialize(NA) # NA は素で logical
      invisible(self)
    }
  )
)
RSNABoolean$isNAClass <- TRUE
