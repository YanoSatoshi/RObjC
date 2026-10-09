#' @include runtime_robjc.r

#' @title Get an Object's Class
#' @description Returns the class generator for an RObjC instance.
#' @param obj An RObjC instance.
#' @return The instance's class generator. Raises an error if `obj` is not an
#'   RObjC instance.
#' @examples
#' robjc:::object_getClass(RSObject$new())$classname
#' @keywords internal
object_getClass <- function(obj) {
  if (!robjc_isRobjcObject(obj))
    stop("`obj` is not a robjc object.", call. = FALSE)
  obj$.__enclos_env__$private$.__isa$class
}
