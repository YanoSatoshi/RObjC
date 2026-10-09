#' @include RSArray.r
#' @title RSArray and RSMutableArray Convenience Constructors
#' @description Construct ordered collections from RObjC objects or an existing
#'   array. The `withObjects` input is an R list of objects.
#' @param object An RObjC object.
#' @param objects A list of RObjC objects.
#' @param array An `RSArray` or `RSMutableArray` instance.
#' @param capacity A non-negative whole number. Capacity reservation is not
#'   implemented for the current R list storage.
#' @return An array of the type named by the function, except that copying an
#'   immutable array may return that same instance.
#' @name array_constructors
NULL

.rs_api_array_objects <- function(objects) {
  if (!is.list(objects) || !all(vapply(objects, robjc_isRobjcObject, logical(1))))
    stop("`objects` must be a list of RObjC objects.", call. = FALSE)
  objects
}

.rs_api_array <- function(array) {
  if (!robjc_isRobjcObject(array) ||
      !array$isKindOf(RSMutableArray))
    stop("`array` must be an RSArray or RSMutableArray.", call. = FALSE)
  array
}

#' @rdname array_constructors
#' @export
RSArray_empty <- function() RSArray$new()

#' @rdname array_constructors
#' @export
RSArray_withObject <- function(object) {
  RSArray$new(objects = .rs_api_array_objects(list(object)))
}

#' @rdname array_constructors
#' @export
RSArray_withObjects <- function(objects) {
  RSArray$new(objects = .rs_api_array_objects(objects))
}

#' @rdname array_constructors
#' @export
RSArray_withArray <- function(array) .rs_api_array(array)$copy()

#' @rdname array_constructors
#' @export
RSMutableArray_empty <- function() RSMutableArray$new()

#' @rdname array_constructors
#' @export
RSMutableArray_withObject <- function(object) {
  RSMutableArray$new(objects = .rs_api_array_objects(list(object)))
}

#' @rdname array_constructors
#' @export
RSMutableArray_withObjects <- function(objects) {
  RSMutableArray$new(objects = .rs_api_array_objects(objects))
}

#' @rdname array_constructors
#' @export
RSMutableArray_withArray <- function(array) .rs_api_array(array)$mutableCopy()

#' @rdname array_constructors
#' @export
RSMutableArray_withCapacity <- function(capacity) {
  if (
    !is.numeric(capacity)   ||
    length(capacity) != 1L  ||
    is.na(capacity)         ||
    !is.finite(capacity)    ||
    capacity < 0            ||
    capacity != floor(capacity)
  ) stop("`capacity` must be a non-negative whole number.", call. = FALSE)
  stop("Capacity reservation is not implemented.", call. = FALSE)
}
