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
#' @section Usage:
#' Use \code{RSDate$dateWith(val)} to construct safely from a Date scalar.
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

    #' @description
    #' Constructs a new `RSDate` object by wrapping a scalar Date value.
    #' If no value is provided, the current system date (`Sys.Date()`) is used as default.
    #' The input is validated to ensure it is of class `"Date"`, and an error is raised otherwise.
    #' This method delegates scalar validation and storage to the superclass `RSScalar`.
    #' 
    #' @param val A scalar object of class `"Date"`.
    #' @return For `initialize()`: The constructed `RSDate` object (invisible).
    initialize = function(val = Sys.Date()) {
      if (!inherits(val, "Date"))   stop("val must be of class Date")
      super$initialize(val)
      invisible(self)
    },


    #' @description
    #' Computes an integer hash value based on the numeric representation of the internal `Date` value.
    #' This enables consistent use of `RSDate` objects in hashed containers such as sets or dictionaries.
    #' 
    #' @return For `hash()`: An integer hash corresponding to the number of days since 1970-01-01.
    hash = function() {
      as.integer(as.numeric(private$.val))
    }
  )
)
RSDate$isNAClass <- FALSE





#' @title RSNADate: Wrapper for NA Date Scalar
#'
#' @description
#' `RSNADate` is a subclass of `RSDate` that represents `as.Date(NA)` in a scalar-wrapper form.
#' This class is used internally to distinguish between regular and missing date values
#' within scalar-aware containers and comparison logic.
#'
#' Normally constructed via `RSDate$dateWith()`.
#'
#' @examples
#' na_date <- RSDate$dateWith(as.Date(NA))
#' na_date$isKindOf(RSDate)  # TRUE
#' na_date$value()           # NA
#'
#' @export
RSNADate <- R6::R6Class(
  "RSNADate",
  cloneable = FALSE,
  inherit = RSDate,
  public = list(

    #' @description
    #' Initializes an `RSNADate` object that wraps an `NA` logical value.
    #' This class represents the NA counterpart to `RSDate`, and should be used
    #' whenever a logical `NA` is required within the scalar object framework.
    #' Internally, it delegates to `RSDate$initialize(NA)` and returns the object invisibly.
    initialize = function() {
      super$initialize(as.Date(NA))
      invisible(self)
    }
  )
)
RSNADate$isNAClass <- TRUE



# @title Factory Method for RSDate
# 
# @name dateWith
#
# @description
# Creates an `RSDate` object from a `Date` scalar. If the input is `as.Date(NA)`,
# it returns an instance of `RSNADate`.
#
# @param val A `Date` scalar.
# @return An instance of `RSDate` or `RSNADate`.
#
# @examples
# RSDate$dateWith(as.Date("2023-01-01"))
# RSDate$dateWith(as.Date(NA))
# @export
RSDate$dateWith  <- function(val) {
  if (is.na(val)) return(RSNADate$new())
  return(RSDate$new(val))
}