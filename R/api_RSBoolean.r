#' @include RSBoolean.r
#' @title RSBoolean Convenience Constructor
#' @description Wrap a logical scalar. Missing values produce [RSNABoolean].
#' @param val A logical scalar without an S3 class attribute, including `NA`.
#' @return An [RSBoolean] or [RSNABoolean] instance.
#' @export
RSBoolean_withBoolean <- function(val) {
  if (!inherits(val, "logical") || length(val) != 1L || !is.null(dim(val)))
    stop("`val` must be a logical scalar.", call. = FALSE)
  if (is.object(val))
    stop("S3 class attributes are not supported for RSBoolean.", call. = FALSE)
  if (is.na(val)) return(RSBoolean$na)
  RSBoolean$new(val)
}
