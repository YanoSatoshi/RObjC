#' @importFrom utils capture.output
#' 
#' @title Get Environment Pointer Hex for R6 or Function Objects
#'
#' @description
#' Returns a hexadecimal string representing the memory address of an R6 object's
#' bound method environment (typically the \code{$print} method), or produces
#' an informative error if the pointer cannot be resolved.
#'
#' This function is primarily used within the robjc framework to generate
#' stable identity keys (e.g., for hashing or visited tracking) based on internal
#' object environments.
#'
#' If the input object is an R6 instance (typically inheriting from \code{RSObject}),
#' its environment is extracted from the bound \code{$print} method.
#' For R6 class definitions (\code{R6ClassGenerator}), the environment of the
#' class-level \code{$print} may also be used.
#'
#' If the object is a function or closure that lacks a \code{$print} method,
#' the function fails gracefully with a label showing the name or expression passed.
#'
#' @param some_obj An object, typically an R6 instance or R6 class generator. It may
#'   also be a plain function. If a function is passed, a helpful error message will
#'   be shown when the pointer cannot be extracted.
#'
#' @param obj_name (Optional) A character string indicating the object name or
#'   source expression. This is auto-filled by \code{deparse(substitute(...))} when omitted.
#'   Only used for more informative error reporting.
#'
#' @return A character string like \code{"0x55abcde12345"} representing the object's internal environment pointer.
#'
#' @examples
#' \dontrun{
#' obj <- RSObject$new()
#' get_pointer_hex(obj)             # e.g., "0x55abcde12345"
#'
#' get_pointer_hex(RSObject)        # class generator
#'
#' get_pointer_hex(mean)            # → error with "function mean"
#'
#' get_pointer_hex(function(x) x)   # → error with full expression
#' }
#'
#' @seealso \code{\link[=RSObject]{objectID}}, \code{\link{RSObject}}, \code{\link{RSClassEncoder}}
#'
#' @export
get_pointer_hex <- function(some_obj, obj_name = NULL) {
  if (is.null(obj_name)) {
    obj_name <- tryCatch(
      deparse(substitute(some_obj)), error = function(e) "unknown"
    )
  }
  get_label <- function() {
    tryCatch({
      if (robjc_isRobjcObject(some_obj)) {
        some_obj$class()$classname
      } else if (robjc_isRobjcClass(some_obj)) {
        some_obj$classname
      } else if (is.function(some_obj)) {
        paste0("function ", obj_name)
      } else {
        class(some_obj)[1]
      }
    }, error = function(e) paste("unknown object (", obj_name, ")"))
  }
  env <- tryCatch(environment(some_obj$print), error = function(e) NULL)
  if (is.null(env)) {
    label <- get_label()
    stop(sprintf("Pointer could not be resolved: object = [%s]", label))
  }
  addr <- capture.output(print(env))[1]
  hex_match <- sub("^<environment: (0x[0-9a-f]+)>$", "\\1", addr)
  if (is.na(hex_match) || !nzchar(hex_match)) {
    label <- tryCatch({
      cls_name <- get_label()
      env_str <- addr
      paste0(cls_name, " @ ", env_str)
    }, error = function(e) "unknown")
    stop(
      sprintf("Internal pointer (.ptr) could not be extracted for %s.", label)
    )
  }
  return(hex_match)
}













