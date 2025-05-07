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
#' @examples
#' dt <- RSDateTime$dateTimeWith(Sys.time())
#' dt$value()
#' dt$hash()
#'
#' @seealso RSNADateTime
#'
#' @export
RSDateTime <- R6::R6Class(
  ### class name ###
  "RSDateTime",
  cloneable = FALSE,
  inherit = RSScalar,
  public = list(
    initialize = function(val = Sys.time()) {
      if (!inherits(val, "POSIXct")) stop("val must be of class POSIXct")
      # タイムゾーンを固定
      val <- as.POSIXct(val, tz = "Asia/Tokyo")
      super$initialize(val)
      invisible(self)
    },
    hash = function() {
      # UNIX時間（秒単位）を整数に変換してハッシュに
      as.integer(as.numeric(private$.val))
    }
  )
)
RSDateTime$isNAClass <- FALSE




#' @title Factory Method for RSDateTime
#'
#' @description
#' Creates an `RSDateTime` object from a `POSIXct` scalar. If the value is
#' `NA`, returns an `RSNADateTime` instance instead.
#'
#' @param val A `POSIXct` timestamp scalar.
#' @return An instance of `RSDateTime` or `RSNADateTime`.
#'
#' @export
RSDateTime$dateTimeWith <- function(val) {
  if (is.na(val)) return(RSNADateTime$new())
  return(RSDateTime$new(val))
}





#' @title RSNADateTime: Wrapper for NA POSIXct Timestamp
#'
#' @description
#' `RSNADateTime` is a subclass of `RSDateTime` used to represent missing datetime values.
#' It ensures that `NA` timestamps behave correctly in comparison, hashing, and collection operations.
#'
#' This class is typically constructed via `RSDateTime$dateTimeWith()`.
#'
#' @examples
#' na_dt <- RSDateTime$dateTimeWith(NA_POSIXct_)
#' na_dt$isKindOf(RSDateTime)  # TRUE
#' na_dt$value()               # NA
#'
#' @export
RSNADateTime <- R6::R6Class(
  "RSNADateTime",
  cloneable = FALSE,
  inherit = RSDateTime,
  public = list(
    initialize = function() {
      super$initialize(NA_POSIXct_)
      invisible(self)
    }
  )
)
RSNADateTime$isNAClass <- TRUE
