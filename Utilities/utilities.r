#' @title Cast RY Storage Objects to List
#'
#' @description
#' Attempts to convert an RYObject-based storage object to a standard list using its `asList()` method,
#' if supported. If the input is not an RYObject or does not respond to `asList`, it is returned unchanged.
#'
#' @param objects An object which may or may not be an instance of `RYObject`.
#' @return A `list` if conversion is successful; otherwise, returns the input object as-is.
#' @examples
#' # Assuming `x` is an RYObject that supports asList:
#' # cast_ry_storage_to_list(x)
#' @export
cast_ry_storage_to_list <- function(objects) {
  if (
    !is.null(objects$isKindOf) &&
    objects$isKindOf(RYObject) && # nolint
    objects$respondsTo("asList")
  )  return(objects$asList())
  return(objects)
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
