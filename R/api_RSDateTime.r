#' @include RSDateTime.r
#' @title RSDateTime Convenience Constructors
#' @description Construct a date-time wrapper. `NA` produces [RSNADateTime],
#'   while `NaN` remains an [RSDateTime] value.
#' @param val A scalar with the standard `POSIXct` or `POSIXlt` class,
#'   including `NA` and `NaN`.
#' @return An [RSDateTime] or [RSNADateTime] instance.
#' @name datetime_constructors
NULL

#' @rdname datetime_constructors
#' @export
RSDateTime_now <- function() RSDateTime$new(Sys.time())

#' @rdname datetime_constructors
#' @export
RSDateTime_withDateTime <- function(val) {
  standard_class <- identical(class(val), c("POSIXct", "POSIXt")) ||
    identical(class(val), c("POSIXlt", "POSIXt"))
  if (!standard_class || length(val) != 1L || !is.null(dim(val)))
    stop("`val` must be a POSIXct or POSIXlt scalar.", call. = FALSE)
  normalized <- as.POSIXct(val)
  if (!inherits(normalized, "POSIXct") || length(normalized) != 1L ||
      !is.null(dim(normalized)) ||
      !(typeof(normalized) %in% c("integer", "double")))
    stop("`val` must be a numeric POSIX date-time.", call. = FALSE)
  if (is.na(val) && !is.nan(val)) return(RSDateTime$na)
  RSDateTime$new(val)
}
