#' @include RSRaw.r
#' @title Wrap a Raw Scalar
#' @description Wrap one base R raw byte. Raw values have no NA companion.
#' @param val A length-one raw value without an S3 class attribute.
#' @return An [RSRaw] instance.
#' @export
RSRaw_withValue <- function(val) {
  if (!is.raw(val) || length(val) != 1L || !is.null(dim(val)))
    stop("`val` must be a raw scalar.", call. = FALSE)
  if (is.object(val))
    stop("S3 class attributes are not supported for RSRaw.", call. = FALSE)
  RSRaw$new(val)
}
