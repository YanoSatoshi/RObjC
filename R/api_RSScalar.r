#' @include api_RSNull.r
#' @include api_RSBoolean.r
#' @include api_RSNumber.r
#' @include api_RSComplex.r
#' @include api_RSRaw.r
#' @include api_RSString.r
#' @include api_RSDate.r
#' @include api_RSDateTime.r
#' @title Wrap a Native R Scalar
#' @description Choose the matching RObjC scalar class for a native R value.
#'   `NULL` returns the shared [RSNull]; `NA` uses the matching `RSNA*`
#'   class through the typed API, while supported `NaN` values remain in their
#'   regular scalar classes. Built-in scalar wrappers accept base R classes,
#'   not custom S3 subclasses.
#' @param val `NULL` or a length-one, dimensionless logical, integer, double, complex, raw,
#'   character, factor, `Date`, `POSIXct`, or `POSIXlt` value with its standard
#'   base R class.
#' @return An RObjC scalar instance.
#' @export
RSScalar_withValue <- function(val) {
  if (is.null(val))             return(RSNull_null())
  if (!is.null(dim(val))) stop("`val` must be a scalar.", call. = FALSE)
  if (length(val) != 1L)        stop("`val` must have length one.", call. = FALSE)
  if (inherits(val, "POSIXt"))  return(RSDateTime_withDateTime(val))
  if (inherits(val, "Date"))    return(RSDate_withDate(val))
  if (is.factor(val))           return(RSFactor_withFactor(val))
  if (is.logical(val))          return(RSBoolean_withBoolean(val))
  if (is.character(val))        return(RSString_withValue(val))
  if (is.complex(val))          return(RSComplex_withValue(val))
  if (is.raw(val))              return(RSRaw_withValue(val))
  if (is.numeric(val) && typeof(val) %in% c("integer", "double")) return(RSNumber_withValue(val))
  stop(
    sprintf("Unsupported scalar type: %s", class(val)[1L]),
    call. = FALSE
  )
}
