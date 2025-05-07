#' @title RSNumber: Numeric Scalar Wrapper
#'
#' @description
#' `RSNumber` wraps a numeric scalar (double or integer) in a value-object style,
#' inheriting common scalar behavior from `RSScalar`.
#' It disallows `NA` values directly; use `RSNANumber` instead.
#'
#' @section Usage:
#' Use `RSNumber$numberWith(val)` to construct an instance safely.
#'
#' @section See Also:
#' [RSNANumber] for handling NA values.
#'
#' @export
RSNumber <- R6::R6Class(
  "RSNumber",
  cloneable = FALSE,
  inherit = RSScalar,
  public = list(
    initialize = function(val) {
      if (!is.numeric(val) || is.na(val)) {
        stop("val must be of class numeric and not NA")
      }
      super$initialize(val)
      invisible(self)
    }
  )
)
RSNumber$isNAClass  <- FALSE

#' @title RSNumber: Numeric Scalar Wrapper
#'
#' @description
#' `RSNumber` wraps a numeric scalar (double or integer) in a value-object style,
#' inheriting common scalar behavior from `RSScalar`.
#' It disallows `NA` values directly; use `RSNANumber` instead.
#'
#' @section Usage:
#' Use `RSNumber$numberWith(val)` to construct an instance safely.
#'
#' @section See Also:
#' [RSNANumber] for handling NA values.
#'
#' @export
RSNumber$numberWith <- function(val) {
  if (is.na(val)) return(RSNANumber$new())
  return(RSNumber$new(val))
}




#' @title RSNANumber: Wrapper for NA-numeric Scalar
#'
#' @description
#' `RSNANumber` is a subclass of `RSNumber` used specifically to represent `NA_real_` values
#' in a safe, class-distinct manner. Instances of this class are recognized by `isNAClass == TRUE`.
#'
#' @note
#' This class should not be instantiated directly under most circumstances.
#' Use `RSNumber$numberWith(val)` to safely wrap numeric values.
#'
#' @export
RSNANumber <- R6::R6Class(
  "RSNANumber",
  cloneable = FALSE,
  inherit = RSNumber,
  public = list(
    initialize = function() {
      super$initialize(NA_real_)
      invisible(self)
    }
  )
)
RSNANumber$isNAClass <- TRUE
