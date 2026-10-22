#' @title Cast RS Storage Objects to List
#'
#' @description
#' Returns an input list unchanged. Otherwise, accepts an RObjC instance with
#' a public `asList()` method and requires that method to return a list.
#' `NULL`, other inputs, and non-list `asList()` results are rejected.
#'
#' @param collections A list or a RObjC collection that provides `asList()`.
#' @return A list, including `list()` for an empty collection.
#' @examples
#' # Assuming `x` is an RSObject that supports asList:
#' # cast_collections_to_list(x)
#' 
#' @keywords internal
cast_collections_to_list <- function(collections) {
  if (is.list(collections)) return(collections)
  if (!robjc_isRobjcObject(collections))
    stop("`collections` must be a list or RObjC instance.", call. = FALSE)
  if (!collections$respondsTo("asList"))
    stop("`collections` must provide `asList()`.", call. = FALSE)
  result <- collections$asList()
  if (!is.list(result))
    stop("`asList()` must return a list.", call. = FALSE)
  result
}



#' @title Error Message for Immutable Object Mutation Attempts
#'
#' @description
#' Generates a standardized error message for an immutable RS class that has
#' a corresponding `RSMutable*` class. It suggests that mutable class instead.
#'
#' @param klass A class object that includes a `classname` field.
#' @return A character string containing the error message.
#' @examples
#' # msg_immutable_try_mutate(RSArray)
#' @keywords internal
msg_immutable_try_mutate <- function(klass) {
  cls_name <- klass$classname
  sprintf(
    "%s is immutable. Use %s for modification.",
    cls_name, sub("RS", "RSMutable", cls_name)
  )
}
