#' @include RSBoolean.r
#' @title RSBoolean Convenience Constructor
#' @description Wrap a logical scalar. Missing values produce [RSNABoolean].
#' @param val A logical scalar, including `NA`.
#' @return An [RSBoolean] or [RSNABoolean] instance.
#' @export
RSBoolean_withBoolean <- function(val) {
  if (!is.logical(val) || length(val) != 1L)
    stop("`val` must be a logical scalar.", call. = FALSE)
  if (is.na(val)) return(RSBoolean$na)
  RSBoolean$new(val)
}
