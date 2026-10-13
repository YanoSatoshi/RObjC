#' @include RSRaw.r
#' @title Wrap a Raw Scalar
#' @description Wrap one base R raw byte. Raw values have no NA companion.
#' @param val A length-one raw value.
#' @return An [RSRaw] instance.
#' @export
RSRaw_withValue <- function(val) {
  if (!is.raw(val) || length(val) != 1L)
    stop("`val` must be a raw scalar.", call. = FALSE)
  RSRaw$new(val)
}
