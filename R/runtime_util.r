#' @importFrom utils capture.output
#' 
#' @title Get an R6 Object's Environment Address
#'
#' @description
#' Returns a hexadecimal address for the environment enclosing an R6 object's
#' `$print` method. The address can identify an object during the current R
#' session; it is not a persistent identifier across sessions.
#'
#' RObjC uses this address for in-session object identity checks. An object
#' without a `$print` method raises an error.
#'
#' @param some_obj An R6 instance or class generator with a `$print` method.
#' @param obj_name Optional label used in error messages. By default, the input
#'   expression is captured.
#'
#' @return A character string containing a hexadecimal address, such as
#'   `"0x55abcde12345"`.
#'
#' @examples
#' obj <- RSObject$new()
#' robjc:::get_pointer_hex(obj)
#' robjc:::get_pointer_hex(RSObject)
#'
#' @seealso [RSObject]
#'
#' @keywords internal
get_pointer_hex <- function(some_obj, obj_name = NULL) {
  if (is.null(obj_name)) {
    obj_name <- tryCatch(
      deparse(substitute(some_obj)),
      error = function(e) "unknown"
    )
  }
  get_label <- function() {
    tryCatch(
      {
        if (robjc_isRobjcObject(some_obj))
          some_obj$class()$classname
        else if (robjc_isMetaClass(some_obj))
          some_obj$classname
        else if (is.function(some_obj))
          paste0("function ", obj_name)
        else
          class(some_obj)[1]
      },
      error = function(e) paste0("unknown object (", obj_name, ")")
    )
  }
  env <- tryCatch(
    environment(some_obj$print),
    error = function(e) NULL
  )
  if (is.null(env)) {
    label <- get_label()
    stop(
      sprintf("Pointer could not be resolved: object = [%s]", label),
      call. = FALSE
    )
  }
  addr <- utils::capture.output(print(env))[1]
  hex_match <- sub("^<environment: (0x[0-9a-f]+)>$", "\\1", addr)
  if (is.na(hex_match) || !nzchar(hex_match)) {
    label <- tryCatch({
        cls_name <- get_label()
        env_str <- addr
        paste0(cls_name, " @ ", env_str)
      },
      error = function(e) "unknown"
    )
    stop(
      sprintf("Internal pointer (.ptr) could not be extracted for %s.", label),
      call. = FALSE
    )
  }
  return(hex_match)
}
