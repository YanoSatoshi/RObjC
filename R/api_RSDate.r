#' @include RSDate.r
#' @title RSDate Convenience Constructors
#' @description Construct a date wrapper. `NA` produces [RSNADate], while
#'   `NaN` remains an [RSDate] value.
#' @param val A scalar with the standard `Date` class, including `NA` and `NaN`.
#' @return An [RSDate] or [RSNADate] instance.
#' @name date_constructors
NULL

#' @rdname date_constructors
#' @export
RSDate_today <- function() RSDate$new(Sys.Date())

#' @rdname date_constructors
#' @export
RSDate_withDate <- function(val) {
  if (!identical(class(val), "Date") || length(val) != 1L || !is.null(dim(val)))
    stop("`val` must be a Date scalar.", call. = FALSE)
  if (!(typeof(val) %in% c("integer", "double")))
    stop("`val` must be a numeric Date.", call. = FALSE)
  if (is.na(val) && !is.nan(val)) return(RSDate$na)
  RSDate$new(val)
}
