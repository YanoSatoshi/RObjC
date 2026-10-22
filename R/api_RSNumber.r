#' @include RSNumber.r
#' @title RSNumber Convenience Constructors
#' @description Wrap a numeric scalar. `NA` produces [RSNANumber]; `NaN`
#'   remains an [RSNumber] value.
#' @param val An integer or double scalar without an S3 class attribute,
#'   including `NA` and `NaN`.
#' @param value An unclassed integer or double scalar as specified by the function name.
#' @return An [RSNumber] or [RSNANumber] instance.
#' @name number_constructors
NULL

#' @rdname number_constructors
#' @export
RSNumber_withValue <- function(val) {
  if (!is.numeric(val) || length(val) != 1L || !is.null(dim(val)) ||
      !(typeof(val) %in% c("integer", "double")))
    stop("`val` must be an integer or double scalar.", call. = FALSE)
  if (is.object(val))
    stop("S3 class attributes are not supported for RSNumber.", call. = FALSE)
  if (is.na(val) && !is.nan(val)) return(RSNumber$na)
  RSNumber$new(val)
}

#' @rdname number_constructors
#' @export
RSNumber_withInteger <- function(value) {
  if (!is.integer(value) || length(value) != 1L)
    stop("`value` must be an integer scalar.", call. = FALSE)
  RSNumber_withValue(value)
}

#' @rdname number_constructors
#' @export
RSNumber_withDouble <- function(value) {
  if (!is.double(value) || length(value) != 1L)
    stop("`value` must be a double scalar.", call. = FALSE)
  RSNumber_withValue(value)
}
