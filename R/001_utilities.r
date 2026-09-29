#' @title Cast RS Storage Objects to List
#'
#' @description
#' Attempts to convert an RSObject-based storage object to a standard list using its `asList()` method,
#' if supported. If the input is not an RSObject or does not respond to `asList`, it is returned unchanged.
#'
#' @param objects An object which may or may not be an instance of `RSObject`.
#' @return A `list` if conversion is successful; otherwise, returns the input object as-is.
#' @examples
#' # Assuming `x` is an RSObject that supports asList:
#' # cast_rs_storage_to_list(x)
#' 
#' @keywords internal
cast_rs_storage_to_list <- function(objects) {
  if (is.list(objects)) return(objects)
  stopifnot("Object is not kind of RSObject" = is_robjc_instance(objects))
  if (objects$respondsTo("asList")) {
    return(objects$asList())
  } else {return(NULL)}
}



#' @title Error Message for Immutable Object Mutation Attempts
#'
#' @description
#' Generates a standardized error message when attempting to modify an immutable RS class.
#' It suggests using the corresponding `RSMutable*` class instead.
#'
#' @param klass A class object that includes a `classname` field.
#' @return A character string containing the error message.
#' @examples
#' # msg_immutable_try_mutate(RSNumber)
#' @export
msg_immutable_try_mutate <- function(klass) {
  cls_name <- klass$classname
  sprintf(
    "%s is immutable. Use %s for modification.",
    cls_name, sub("RS", "RSMutable", cls_name)
  )
}
