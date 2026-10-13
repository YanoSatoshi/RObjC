#' @include RSScalar.r
#' @title RSComplex: Complex Scalar Wrapper
#'
#' @description
#' `RSComplex` wraps one base R complex value. Missing values use the shared
#' [RSNAComplex] instance, available as `RSComplex$na`.
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
    #' @param val A length-one complex value.
    #' @return The initialized instance, invisibly.
    initialize = function(val) {
      super$initialize(val)
      if (!is.complex(val) || length(val) != 1L)
        stop("`val` must be a complex scalar.", call. = FALSE)
      invisible(self)
    }
  )
)

#' @title RSNAComplex
#' @description The generated NA companion class for [RSComplex]. Use
#'   `RSComplex$na` for its shared instance.
#' @return The NA class generator.
#' @export
RSNAComplex <- RSComplex$na$class()
