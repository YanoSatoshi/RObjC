#' @include RSDateTime.r
#' @title RSDateTime Convenience Constructors
#' @description Construct a date-time wrapper. A missing timestamp produces
#'   [RSNADateTime].
#' @param val A `POSIXct` scalar, including a missing timestamp.
#' @return An [RSDateTime] or [RSNADateTime] instance.
#' @name datetime_constructors
NULL

#' @rdname datetime_constructors
#' @export
RSDateTime_now <- function() RSDateTime$new(Sys.time())

#' @rdname datetime_constructors
#' @export
RSDateTime_withDateTime <- function(val) {
  if (!inherits(val, "POSIXct") || length(val) != 1L)
    stop("`val` must be a POSIXct scalar.", call. = FALSE)
  if (is.na(val)) return(RSDateTime$na)
  RSDateTime$new(val)
}
