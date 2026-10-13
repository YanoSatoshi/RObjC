#' @include RSComplex.r
#' @title Wrap a Complex Scalar
#' @description Wrap one base R complex value. Missing values return the shared
#'   [RSNAComplex] instance.
#' @param val A length-one complex value.
#' @return An [RSComplex] or [RSNAComplex] instance.
#' @export
RSComplex_withValue <- function(val) {
  if (!is.complex(val) || length(val) != 1L)
    stop("`val` must be a complex scalar.", call. = FALSE)
  if (is.na(val)) return(RSComplex$na)
  RSComplex$new(val)
}
