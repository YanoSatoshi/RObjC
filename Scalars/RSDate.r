#' @title RSDate: Scalar Wrapper for Date Values
#'
#' @description
#' `RSDate` is a scalar wrapper class for R's `Date` objects.
#' It inherits from `RSScalar` and supports equality checking, hashing, and
#' use in hash-based containers such as `RSSet` and `RSDictionary`.
#'
#' The constructor ensures that the value is of class `"Date"`. To construct
#' safely from a value that may include `NA`, use the factory method `RSDate$dateWith()`.
#'
#' @section Methods:
#' - `hash()` – Converts the date to an integer representation for hashing.
#'
#' @examples
#' d <- RSDate$dateWith(Sys.Date())
#' d$value()
#' d$hash()
#'
#' @seealso RSNADate
#'
#' @export
RSDate <- R6::R6Class(
  ### class name ###
  "RSDate",
  cloneable = FALSE,
  inherit = RSScalar,
  public = list(
    initialize = function(val = Sys.Date()) {
      if (!inherits(val, "Date"))   stop("val must be of class Date")
      super$initialize(val)
      invisible(self)
    },
    hash = function() {
      as.integer(as.numeric(private$.val))
    }
  )
)
RSDate$isNAClass <- FALSE



#' @title Factory Method for RSDate
#'
#' @description
#' Creates an `RSDate` object from a `Date` scalar. If the input is `NA_Date_`,
#' it returns an instance of `RSNADate`.
#'
#' @param val A `Date` scalar.
#' @return An instance of `RSDate` or `RSNADate`.
#'
#' @examples
#' RSDate$dateWith(as.Date("2023-01-01"))
#' RSDate$dateWith(NA_Date_)
#'
#' @export
RSDate$dateWith  <- function(val) {
  if (is.na(val)) return(RSNADate$new())
  return(RSDate$new(val))
}




#' @title RSNADate: Wrapper for NA Date Scalar
#'
#' @description
#' `RSNADate` is a subclass of `RSDate` that represents `NA_Date_` in a scalar-wrapper form.
#' This class is used internally to distinguish between regular and missing date values
#' within scalar-aware containers and comparison logic.
#'
#' Normally constructed via `RSDate$dateWith()`.
#'
#' @examples
#' na_date <- RSDate$dateWith(NA_Date_)
#' na_date$isKindOf(RSDate)  # TRUE
#' na_date$value()           # NA
#'
#' @export
RSNADate <- R6::R6Class(
  "RSNADate",
  cloneable = FALSE,
  inherit = RSDate,
  public = list(
    initialize = function() {
      super$initialize(NA_Date_)
      invisible(self)
    }
  )
)
RSNADate$isNAClass <- TRUE
