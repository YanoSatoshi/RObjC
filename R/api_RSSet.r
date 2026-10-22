#' @include RSSet.r
#' @title RSSet and RSMutableSet Convenience Constructors
#' @description Construct sets from RObjC objects, class or protocol generators,
#'   an array, or an existing set.
#'   `NULL` elements are stored as [RSNull].
#' @param object An RObjC object, class generator, protocol generator, or `NULL`.
#' @param objects A list of RObjC objects, class generators, protocol generators,
#'   or `NULL` elements.
#' @param array An `RSArray` or `RSMutableArray` instance.
#' @param set An `RSSet` or `RSMutableSet` instance.
#' @return A set of the type named by the function, except that copying an
#'   immutable set may return that same instance.
#' @name set_constructors
NULL

.rs_api_set_objects <- function(objects) {
  if (!is.list(objects) || !all(vapply(
    objects,
    function(object) is.null(object) || robjc_isRobjcObject(object) ||
      robjc_isMetaClass(object),
    logical(1)
  )))
    stop("`objects` must be a list of RObjC objects, classes, protocols, or NULL.", call. = FALSE)
  objects
}

.rs_api_set_array <- function(array) {
  if (!robjc_isRobjcObject(array) || !array$isKindOf(RSMutableArray))
    stop("`array` must be an RSArray or RSMutableArray.", call. = FALSE)
  array
}

.rs_api_set <- function(set) {
  if (!robjc_isRobjcObject(set) || !set$isKindOf(RSMutableSet))
    stop("`set` must be an RSSet or RSMutableSet.", call. = FALSE)
  set
}

#' @rdname set_constructors
#' @export
RSSet_empty <- function() RSSet$new()

#' @rdname set_constructors
#' @export
RSSet_withObject <- function(object) RSSet$new(objects = .rs_api_set_objects(list(object)))

#' @rdname set_constructors
#' @export
RSSet_withObjects <- function(objects) RSSet$new(objects = .rs_api_set_objects(objects))

#' @rdname set_constructors
#' @export
RSSet_withArray <- function(array) {
  RSSet$new(objects = .rs_api_set_objects(.rs_api_set_array(array)$asList()))
}

#' @rdname set_constructors
#' @export
RSSet_withSet <- function(set) .rs_api_set(set)$copy()

#' @rdname set_constructors
#' @export
RSMutableSet_empty <- function() RSMutableSet$new()

#' @rdname set_constructors
#' @export
RSMutableSet_withObject <- function(object) {
  RSMutableSet$new(objects = .rs_api_set_objects(list(object)))
}

#' @rdname set_constructors
#' @export
RSMutableSet_withObjects <- function(objects) {
  RSMutableSet$new(objects = .rs_api_set_objects(objects))
}

#' @rdname set_constructors
#' @export
RSMutableSet_withArray <- function(array) {
  RSMutableSet$new(objects = .rs_api_set_objects(.rs_api_set_array(array)$asList()))
}

#' @rdname set_constructors
#' @export
RSMutableSet_withSet <- function(set) .rs_api_set(set)$mutableCopy()
