#' @include RSNumber.r
#' @title RSNumber Convenience Constructors
#' @description Wrap a numeric scalar. Missing values produce [RSNANumber].
#' @param val An integer or double scalar, including `NA`.
#' @param value An integer or double scalar as specified by the function name.
#' @return An [RSNumber] or [RSNANumber] instance.
#' @name number_constructors
NULL

#' @rdname number_constructors
#' @export
RSNumber_withValue <- function(val) {
  if (!is.numeric(val) || length(val) != 1L || !(typeof(val) %in% c("integer", "double")))
    stop("`val` must be an integer or double scalar.", call. = FALSE)
  if (is.na(val)) return(RSNANumber$new())
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
