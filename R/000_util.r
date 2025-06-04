#' @importFrom utils capture.output

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
      if (is_robjc_instance(some_obj)) {
        some_obj$class()$classname
      } else if (inherits(some_obj, "R6ClassGenerator")) {
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








#' @title Check if an Object is a robjc-Based Instance
#'
#' @description
#' Determines whether the given object is a valid instance of the `robjc` object-oriented system.
#' This is done by verifying two conditions: the object must implement a `class()` method,
#' and the corresponding class object must possess a `robjc_isa` structure—
#' both of which are characteristic of robjc-based objects.
#'
#' @param obj Any R object.
#'
#' @return `TRUE` if the object is recognized as a robjc object, `FALSE` otherwise.
#'
#' @examples
#' obj <- RSObject$new()
#' is_robjc_instance(obj)  # Returns TRUE
#' is_robjc_instance(list())  # Returns FALSE
#'
#' @export
is_robjc_instance <- function(obj) {
  tryCatch(!is.null(obj$class()$robjc_isa), error = function(e) FALSE)
}









#' @title Check if an Object is a robjc Class Object
#'
#' @description
#' Determines whether the given object is a class definition created within the `robjc` object-oriented system.
#' Specifically, it checks for the presence of the internal `robjc_isa` metadata structure,
#' which is a characteristic of all robjc class objects.
#'
#' @param obj Any R object.
#'
#' @return `TRUE` if the object is recognized as a robjc class object, `FALSE` otherwise.
#'
#' @examples
#' cls <- RSObject
#' is_robjc_class(cls)  # Returns TRUE
#' is_robjc_class(R6::R6Class("NotRobjc"))  # Returns FALSE
#'
#' @export
is_robjc_class <- function(obj) {
  tryCatch(
    {
      if (!is.null(obj$robjc_isa)) return(TRUE)
      super_class <- obj$get_inherit()
      while (!is.null(super_class)) {
        if (identical(super_class, RSObject)) return(TRUE)
        super_class <- super_class$get_inherit()
      }
      return(FALSE)
    },
    error = function(e) FALSE
  )
}
