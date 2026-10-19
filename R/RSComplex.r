#' @include RSScalar.r
#' @title RSComplex: Complex Scalar Wrapper
#'
#' @description
#' `RSComplex` wraps one base R complex value. `NA` values use the shared
#' [RSNAComplex] instance, available as `RSComplex$na`. Values containing
#' `NaN` remain regular `RSComplex` objects.
#'
#' @return The `RSComplex` class generator.
#' @export
RSComplex <- RSScalarClass(
  "RSComplex",
  version = "0.0.3 alpha",
  super_class = RSScalar,
  na_value = NA_complex_,
  public = list(
    #' @description Initialize a complex scalar.
    #' @param val A length-one complex value without an S3 class attribute.
    #' @return The initialized instance, invisibly.
    initialize = function(val) {
      if (!is.complex(val) || length(val) != 1L)
        stop("`val` must be a complex scalar.", call. = FALSE)
      if (is.object(val))
        stop("S3 class attributes are not supported for RSComplex.", call. = FALSE)
      super$initialize(val)
      invisible(self)
    },

    #' @description Compare using base R's complex sort order: real part, then
    #'   imaginary part. Missing values follow `na.last`.
    #' @param other Another `RSComplex` instance.
    #' @param na.last Whether missing values sort last.
    #' @return `-1L`, `0L`, or `1L`.
    compare = function(other, na.last = TRUE) {
      super$compare(other, na.last = na.last)
    }
  )
)

#' @title RSNAComplex
#' @description The generated NA companion class for [RSComplex]. Use
#'   `RSComplex$na` for its shared instance.
#' @return The NA class generator.
#' @export
RSNAComplex <- RSComplex$na$class()
