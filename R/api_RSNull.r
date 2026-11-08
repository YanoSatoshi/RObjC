#' @include RSNull.r
#' @title RSNull Convenience Accessor
#' @description Return the shared `RSNull` instance.
#' @return The shared [RSNull] instance.
#' @examples
#' identical(RSNull_null(), RSNull$null)
#' @export
RSNull_null <- function() RSNull$null
