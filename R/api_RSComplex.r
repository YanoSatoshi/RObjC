#' @include RSComplex.r
#' @title Wrap a Complex Scalar
#' @description Wrap one base R complex value. `NA` values return the shared
#'   [RSNAComplex] instance; values containing `NaN` remain [RSComplex] values.
#' @param val A length-one complex value without an S3 class attribute.
#' @return An [RSComplex] or [RSNAComplex] instance.
#' @examples
#' RSComplex_withValue(1 + 2i)$value()
#' identical(RSComplex_withValue(NA_complex_), RSComplex$na)
#' @export
RSComplex_withValue <- function(val) {
  if (!is.complex(val) || length(val) != 1L || !is.null(dim(val)))
    stop("`val` must be a complex scalar.", call. = FALSE)
  if (is.object(val))
    stop("S3 class attributes are not supported for RSComplex.", call. = FALSE)
  if (is.na(val) && !is.nan(val)) return(RSComplex$na)
  RSComplex$new(val)
}
