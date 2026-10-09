#' @include RSScalar.r
#' @title RSDateTime: Scalar Wrapper for POSIXct Timestamps
#'
#' @description
#' `RSDateTime` wraps POSIXct datetime values as scalar objects, inheriting
#' from `RSScalar`. Time zone is normalized to `"Asia/Tokyo"` to ensure consistency.
#'
#' Values can be hashed and compared, and the class is suitable for use in
#' collections such as `RSSet` or `RSDictionary`.
#'
#' @section Methods:
#' - `hash()` – Uses the numeric representation (UNIX time) as integer hash.
#'
#' #' @section Usage:
#' Use \code{RSDateTime$dateTimeWith(val)} to construct safely from a POSIXct scalar.
#' 
#' @examples
#' dt <- RSDateTime$dateTimeWith(Sys.time())
#' dt$value()
#' dt$hash()
#'
#' @seealso RSNADateTime
#'
#' @export
RSDateTime <- RSScalarClass(
  "RSDateTime",
  super_class = RSScalar,
  is_na_class = FALSE,
  public = list(

    #' @description
    #' Initializes an `RSDateTime` object by wrapping a scalar `POSIXct` value.
    #' If no value is provided, the current system time (`Sys.time()`) is used.
    #' The time zone is always normalized to `"Asia/Tokyo"` for consistency
    #' across internal storage and comparison operations.
    #'
    #' @param val A scalar object of class `"POSIXct"`. Defaults to `Sys.time()`.
    #'
    #' @return An initialized `RSDateTime` object (invisible).
    initialize = function(val = Sys.time()) {
      if (!inherits(val, "POSIXct")) stop("val must be of class POSIXct")
      # タイムゾーンを固定
      val <- as.POSIXct(val, tz = "Asia/Tokyo")
      super$initialize(val)
      invisible(self)
    },


    #' @description
    #' Computes an integer hash based on the numeric UNIX timestamp (in seconds)
    #' of the wrapped `POSIXct` value. This allows consistent hashing and key
    #' usage for date-time objects.
    #'
    #' @return An integer representing the UNIX timestamp as a hash value.
    hash = function() {
      # UNIX時間（秒単位）を整数に変換してハッシュに
      as.integer(as.numeric(private$.val))
    }
  )
)




#' @title RSNADateTime: Wrapper for NA POSIXct Timestamp
#'
#' @description
#' `RSNADateTime` is a subclass of `RSDateTime` used to represent missing datetime values.
#' It ensures that `NA` timestamps behave correctly in comparison, hashing, and collection operations.
#'
#' This class is typically constructed via `RSDateTime$dateTimeWith()`.
#'
#' @examples
#' na_dt <- RSDateTime$dateTimeWith(as.POSIXct(NA))
#' na_dt$isKindOf(RSDateTime)  # TRUE
#' na_dt$value()               # NA
#'
#' @export
RSNADateTime <- RSScalarClass(
  "RSNADateTime",
  super_class = RSDateTime,
  is_na_class = TRUE,
  public = list(

    #' @description
    #' Initializes an `RSNADateTime` object that wraps an `NA` logical value.
    #' This class represents the NA counterpart to `RSDateTime`, and should be used
    #' whenever a logical `NA` is required within the scalar object framework.
    #' Internally, it delegates to `RSDateTime$initialize(NA)` and returns the object invisibly.
    initialize = function() {
      super$initialize(as.POSIXct(NA))
      invisible(self)
    }
  )
)



# @title Factory Method for RSDateTime
#
# @name dateTimeWith
#
# @description
# Creates an `RSDateTime` object from a `POSIXct` scalar. If the value is
# `NA`, returns an `RSNADateTime` instance instead.
#
# @param val A `POSIXct` timestamp scalar.
# @return An instance of `RSDateTime` or `RSNADateTime`.
# @export
RSDateTime$dateTimeWith <- function(val) {
  if (is.na(val)) return(RSNADateTime$new())
  return(RSDateTime$new(val))
}
