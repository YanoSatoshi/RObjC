#' @include RSScalar.r
#' @title RSRaw: Raw Scalar Wrapper
#'
#' @description
#' `RSRaw` wraps one byte as a base R `raw` value. Base R has no missing raw
#' value, so this class has no NA companion.
#'
#' @return The `RSRaw` class generator.
#' @export
RSRaw <- RSObjectClass(
  "RSRaw",
  version = "0.0.4 alpha",
  super_class = RSScalar,
  public = list(
    #' @description Initialize a raw scalar.
    #' @param val A length-one raw value without an S3 class attribute.
    #' @return The initialized instance, invisibly.
    initialize = function(val) {
      if (!is.raw(val) || length(val) != 1L)
        stop("`val` must be a raw scalar.", call. = FALSE)
      if (is.object(val))
        stop("S3 class attributes are not supported for RSRaw.", call. = FALSE)
      super$initialize(val)
      invisible(self)
    },

    #' @description Raw vectors cannot be sorted with base R `sort()` or `order()`.
    #' @param other Another object.
    #' @param na.last Unused; retained for the scalar comparison signature.
    #' @return Never returns; raises an error.
    compare = function(other, na.last = TRUE) {
      stop("Raw values cannot be sorted by base R.", call. = FALSE)
    }
  )
)

RSRaw$isNAClass <- FALSE
# .onLoad() restores this lock on the package-defined generator after lazy loading.
lockBinding("isNAClass", RSRaw)
