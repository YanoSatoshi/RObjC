#' @include RSObject.r RSComparable.r RSEncoding.r
#' @title RSRaw: Raw Byte Wrapper
#'
#' @description
#' `RSRaw` wraps one byte as a base R `raw` value. It inherits directly from
#' [RSObject], not [RSScalar]. Base R has no missing raw value, so this class
#' has no NA companion.
#'
#' @section Protocols:
#' - [RSComparable]
#' - [RSCoding]
#' - [RSCopying]
#'
#' @examples
#' byte <- RSRaw_withValue(as.raw(0xFF))
#' byte$value()
#' byte$isEqual(RSRaw$new(as.raw(0xFF)))
#' byte$isKindOf(RSObject)
#'
#' @return The `RSRaw` class generator.
#' @export
RSRaw <- RSObjectClass(
  "RSRaw",
  version = "0.2.0",
  super_class = RSObject,
  protocols = list(RSComparable, RSCoding, RSCopying),
  public = list(
    #' @description Initialize a raw scalar.
    #' @param val A length-one raw value without an S3 class attribute.
    #' @return The initialized instance, invisibly.
    initialize = function(val) {
      if (!is.raw(val) || length(val) != 1L || !is.null(dim(val)))
        stop("`val` must be a raw scalar.", call. = FALSE)
      if (is.object(val))
        stop("S3 class attributes are not supported for RSRaw.", call. = FALSE)
      super$initialize()
      private$.val <- val
      invisible(self)
    },



    #' @description Encode the wrapped byte.
    #' @param coder An [RSKeyedArchiver].
    #' @return `self`, invisibly.
    encodeWithCoder = function(coder) {
      coder$encodeObjectForKey(private$.val, "value")
      invisible(self)
    },



    #' @description Restore the wrapped byte without calling `initialize()`.
    #' @param coder An [RSKeyedUnarchiver].
    #' @return `self`, invisibly.
    initWithCoder = function(coder) {
      val <- coder$decodeObjectForKey("value")
      if (!is.raw(val) || length(val) != 1L || !is.null(dim(val)) || is.object(val))
        stop("Decoded raw value must be a raw scalar.", call. = FALSE)
      private$.val <- val
      invisible(self)
    },



    #' @description Raw vectors cannot be sorted with base R `sort()` or `order()`.
    #' @param other Another object.
    #' @param na.last Unused; retained for the scalar comparison signature.
    #' @return Never returns; raises an error.
    compare = function(other, na.last = TRUE) {
      stop("Raw values cannot be sorted by base R.", call. = FALSE)
    },



    #' @description Compare the native raw bytes using base R ordering.
    #' @param other Another `RSRaw` instance.
    #' @param na.last Unused; retained for the native comparison signature.
    #' @return `-1L`, `0L`, or `1L`.
    nativeCompare = function(other, na.last = TRUE) {
      if (!robjc_isInstance(other) || !other$isKindOf(RSRaw))
        stop("`other` must be an RSRaw object.", call. = FALSE)
      left <- as.integer(private$.val)
      right <- as.integer(other$value())
      if (left < right) return(-1L)
      if (left > right) return(1L)
      0L
    },



    #' @description Return the wrapped byte as a native R raw value.
    #' @return A length-one raw value.
    value = function() private$.val,



    #' @description Return the wrapped byte as a native R raw value.
    #' @return A length-one raw value.
    nativeValue = function() self$value(),



    #' @description Compare two wrapped raw bytes for value equality.
    #' @param other An object to compare.
    #' @return `TRUE` for the same raw byte, otherwise `FALSE`.
    isEqual = function(other) {
      robjc_isInstance(other) && other$isKindOf(RSRaw) &&
        identical(private$.val, other$value())
    },



    #' @description Hash the wrapped byte.
    #' @return A non-missing integer hash.
    hash = function() {
      robjc_normalize_hash(RSHashOperator$instance$hashForScalar(private$.val))
    },



    #' @description Describe the wrapped byte.
    #' @return A character scalar.
    description = function() {
      sprintf("<%s %s> %s", self$class()$classname,
              robjc_get_pointer(self), as.character(private$.val))
    }
  ),
  private = list(.val = NULL)
)

RSRaw$isNAClass <- FALSE
# .onLoad() restores this lock on the package-defined generator after lazy loading.
lockBinding("isNAClass", RSRaw)
