#' @title object_getClass
#' @description TODO.
#' @noRd
#' @export
object_getClass <- function(obj) {
  if (!robjc_isRobjcObject(obj))
    stop("`obj` is not a robjc object.", call. = FALSE)
  obj$.__enclos_env__$private$.__isa$class
}

