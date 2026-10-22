#' @include RSString.r
#' @include RSFactor.r
#' @title RSString and RSFactor Convenience Constructors
#' @description Construct string and factor wrappers. Missing values produce
#'   the corresponding `RSNA*` class. `RSFactor_withFactor()` creates a new
#'   `RSNAFactor` for each missing input, preserving its levels and ordering.
#' @param val An unclassed character or standard factor scalar for
#'   `RSString_withValue`; a standard factor scalar for `RSFactor_withFactor`.
#' @param string A character scalar or an `RSString` instance.
#' @return The corresponding scalar wrapper.
#' @name string_constructors
NULL

#' @rdname string_constructors
#' @export
RSString_empty <- function() RSString$new("")

#' @rdname string_constructors
#' @export
RSString_withValue <- function(val) {
  valid_class <- (is.character(val) && !is.object(val)) ||
    identical(class(val), "factor") ||
    identical(class(val), c("ordered", "factor"))
  if (!valid_class || length(val) != 1L || !is.null(dim(val))) {
    stop("`val` must be a character or factor scalar.", call. = FALSE)
  }
  if (is.na(val)) return(RSString$na)
  RSString$new(val)
}

#' @rdname string_constructors
#' @export
RSString_withString <- function(string) {
  if (robjc_isRobjcObject(string) && string$isKindOf(RSString)) {
    return(RSString_withValue(as.character(string$value())))
  }
  if (!is.character(string) || length(string) != 1L) {
    stop("`string` must be a character scalar or RSString.", call. = FALSE)
  }
  RSString_withValue(string)
}

#' @rdname string_constructors
#' @export
RSFactor_withFactor <- function(val) {
  standard_class <- identical(class(val), "factor") ||
    identical(class(val), c("ordered", "factor"))
  if (!standard_class || length(val) != 1L || !is.null(dim(val))) {
    stop("`val` must be a factor scalar.", call. = FALSE)
  }
  if (is.na(val)) return(RSNAFactor$new(val))
  RSFactor$new(val)
}
