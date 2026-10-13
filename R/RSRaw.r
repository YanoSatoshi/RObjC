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
  version = "0.0.3 alpha",
  super_class = RSScalar,
  public = list(
    #' @description Initialize a raw scalar.
    #' @param val A length-one raw value.
    #' @return The initialized instance, invisibly.
    initialize = function(val) {
      super$initialize(val)
      if (!is.raw(val) || length(val) != 1L)
        stop("`val` must be a raw scalar.", call. = FALSE)
      invisible(self)
    },

    #' @description Compare the numeric byte values of two raw scalars.
    #' @param other Another `RSRaw` instance.
    #' @param na.last Ignored; raw values cannot be missing.
    #' @return `-1L`, `0L`, or `1L`.
    compare = function(other, na.last = TRUE) {
      if (!is.logical(na.last) || length(na.last) != 1L || is.na(na.last))
        stop("`na.last` must be TRUE or FALSE.", call. = FALSE)
      if (!robjc_isRobjcObject(other) || !other$isKindOf(RSRaw))
        stop("Raw classes are not comparable.", call. = FALSE)
      as.integer(sign(as.integer(private$.val) - as.integer(other$value())))
    }
  )
)

RSRaw$isNAClass <- FALSE
lockBinding("isNAClass", RSRaw)
