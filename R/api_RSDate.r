#' @include RSDate.r
#' @title RSDate Convenience Constructors
#' @description Construct a date wrapper. A missing date produces [RSNADate].
#' @param val A `Date` scalar, including a missing date.
#' @return An [RSDate] or [RSNADate] instance.
#' @name date_constructors
NULL

#' @rdname date_constructors
#' @export
RSDate_today <- function() RSDate$new(Sys.Date())

#' @rdname date_constructors
#' @export
RSDate_withDate <- function(val) {
  if (!inherits(val, "Date") || length(val) != 1L)
    stop("`val` must be a Date scalar.", call. = FALSE)
  if (is.na(val)) return(RSNADate$new())
  RSDate$new(val)
}
