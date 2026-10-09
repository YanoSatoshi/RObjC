#' @include RSString.r
#' @include RSFactor.r
#' @title RSString and RSFactor Convenience Constructors
#' @description Construct string and factor wrappers. Missing values produce
#'   the corresponding `RSNA*` class.
#' @param val A character or factor scalar for `RSString_withValue`; a factor scalar
#'   for `RSFactor_withFactor`.
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
  if (!(is.character(val) || is.factor(val)) || length(val) != 1L) {
    stop("`val` must be a character or factor scalar.", call. = FALSE)
  }
  if (is.na(val)) return(RSNAString$new())
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
  if (!is.factor(val) || length(val) != 1L) {
    stop("`val` must be a factor scalar.", call. = FALSE)
  }
  if (is.na(val)) return(RSNAFactor$new())
  RSFactor$new(val)
}
