#' @include RSDictionary.r
#' @title RSDictionary and RSMutableDictionary Convenience Constructors
#' @description Construct dictionaries from RObjC object or generator keys and
#'   supported values, or an existing dictionary. RObjC object keys must conform
#'   to `RSCopying` because insertion copies them; class and protocol generator
#'   keys are retained by identity. `NULL` keys and values are stored as [RSNull].
#' @param object An RObjC object, class generator, protocol generator, or `NULL`.
#' @param key An RObjC object key conforming to `RSCopying`, a class or protocol
#'   generator, or `NULL`.
#' @param objects A list of RObjC objects, class generators, protocol generators,
#'   or `NULL` values.
#' @param keys A list of RObjC keys conforming to `RSCopying`, class or protocol
#'   generators, or `NULL` keys.
#' @param objectsAndKeys An even-length list alternating value, key, value, key.
#' @param dictionary An `RSDictionary` or `RSMutableDictionary` instance.
#' @return A dictionary of the type named by the function, except that copying
#'   an immutable dictionary may return that same instance.
#' @name dictionary_constructors
NULL

.rs_api_dictionary_value <- function(object) {
  if (!is.null(object) && !robjc_isRobjcObject(object) && !robjc_isClass(object) &&
      !robjc_isProtocol(object))
    stop("Dictionary values must be RObjC objects, classes, protocols, or NULL.",
         call. = FALSE)
  object
}

.rs_api_dictionary_key <- function(key) {
  if (!is.null(key) &&
      !robjc_isMetaClass(key) &&
      (!robjc_isRobjcObject(key) || !isTRUE(key$conformsTo(RSCopying))))
    stop(
      "Dictionary keys must be copyable RObjC objects, classes, protocols, or NULL.",
      call. = FALSE
    )
  key
}

.rs_api_dictionary_pairs <- function(objects, keys) {
  if (!is.list(objects) || !is.list(keys))
    stop("`objects` and `keys` must be lists.", call. = FALSE)
  if (length(objects) != length(keys))
    stop("`objects` and `keys` must have the same length.", call. = FALSE)
  lapply(objects, .rs_api_dictionary_value)
  lapply(keys, .rs_api_dictionary_key)
  invisible(TRUE)
}

.rs_api_dictionary_alternating <- function(objectsAndKeys) {
  if (!is.list(objectsAndKeys) || length(objectsAndKeys) %% 2L != 0L)
    stop("`objectsAndKeys` must be an even-length list of values and keys.", call. = FALSE)
  positions <- seq_along(objectsAndKeys)
  .rs_api_dictionary_pairs(
    objectsAndKeys[positions %% 2L == 1L],
    objectsAndKeys[positions %% 2L == 0L]
  )
  objectsAndKeys
}

.rs_api_dictionary <- function(dictionary) {
  if (!robjc_isRobjcObject(dictionary) ||
      !dictionary$isKindOf(RSMutableDictionary)) {
    stop("`dictionary` must be an RSDictionary or RSMutableDictionary.",
         call. = FALSE)
  }
  dictionary
}

#' @rdname dictionary_constructors
#' @export
RSDictionary_empty <- function() RSDictionary$new()

#' @rdname dictionary_constructors
#' @export
RSDictionary_withObjectForKey <- function(object, key) {
  .rs_api_dictionary_value(object)
  .rs_api_dictionary_key(key)
  RSDictionary$new(objects_and_keys = list(object, key))
}

#' @rdname dictionary_constructors
#' @export
RSDictionary_withObjectsForKeys <- function(objects, keys) {
  .rs_api_dictionary_pairs(objects, keys)
  RSDictionary$new(keys = keys, values = objects)
}

#' @rdname dictionary_constructors
#' @export
RSDictionary_withObjectsAndKeys <- function(objectsAndKeys) {
  RSDictionary$new(objects_and_keys =
                     .rs_api_dictionary_alternating(objectsAndKeys))
}

#' @rdname dictionary_constructors
#' @export
RSDictionary_withDictionary <- function(dictionary) {
  .rs_api_dictionary(dictionary)$copy()
}

#' @rdname dictionary_constructors
#' @export
RSMutableDictionary_empty <- function() RSMutableDictionary$new()

#' @rdname dictionary_constructors
#' @export
RSMutableDictionary_withObjectForKey <- function(object, key) {
  .rs_api_dictionary_value(object)
  .rs_api_dictionary_key(key)
  RSMutableDictionary$new(objects_and_keys = list(object, key))
}

#' @rdname dictionary_constructors
#' @export
RSMutableDictionary_withObjectsForKeys <- function(objects, keys) {
  .rs_api_dictionary_pairs(objects, keys)
  RSMutableDictionary$new(keys = keys, values = objects)
}

#' @rdname dictionary_constructors
#' @export
RSMutableDictionary_withObjectsAndKeys <- function(objectsAndKeys) {
  RSMutableDictionary$new(objects_and_keys =
                            .rs_api_dictionary_alternating(objectsAndKeys))
}

#' @rdname dictionary_constructors
#' @export
RSMutableDictionary_withDictionary <- function(dictionary) {
  .rs_api_dictionary(dictionary)$mutableCopy()
}
