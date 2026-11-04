#' @include runtime_robjc.r


#' @title Get an Object's Class
#' @description Returns the class generator for an RObjC instance.
#' @param obj An RObjC instance.
#' @return The instance's class generator. Raises an error if `obj` is not an
#'   RObjC instance.
#' @examples
#' robjc:::object_getClass(RSObject$new())$classname
#' @keywords internal
object_getClass <- function(obj) {
  if (!robjc_isInstance(obj))
    stop("`obj` is not a robjc object.", call. = FALSE)
  obj$.__enclos_env__$private$.__isa$class
}

#' Wrap a Null Collection Item
#'
#' Converts R `NULL` to the shared `RSNull$null` instance for storage or lookup
#' in an RObjC collection. An RObjC instance, class generator, or protocol
#' generator is returned unchanged. Other values are rejected.
#'
#' @param object An RObjC object, class generator, protocol generator, or `NULL`.
#' @return `RSNull$null` for `NULL`; otherwise `object`.
#' @examples
#' identical(robjc_wrap_null(NULL), RSNull$null)
#' @export
robjc_wrap_null <- function(object) {
  if (is.null(object)) return(RSNull$null)
  if (robjc_isInstance(object) || robjc_isMetaClass(object)) return(object)
  stop(sprintf("Unsupported object type: %s", class(object)[1]), call. = FALSE)
}

.robjc_collection_object <- function(object) {
  if (is.null(object))
    stop("R NULL is not a collection object; use RSNull$null or robjc_wrap_null(NULL).",
         call. = FALSE)
  if (robjc_isInstance(object) || robjc_isMetaClass(object)) return(object)
  stop(sprintf("Unsupported object type: %s", class(object)[1]), call. = FALSE)
}

# Summarize R6 member lists without expanding their values or evaluating methods.
robjc_member_summaries <- function(
  members,
  exclude = character(),
  include_methods = TRUE
) {
  if (is.null(members) || length(members) == 0L)
    return(character())
  names <-
    if (is.environment(members))
      ls(members, all.names = TRUE)
    else
      names(members)
  names <- setdiff(names, exclude)
  if (!include_methods) {
    names <- names[vapply(names, function(name) {
      if (is.environment(members) && bindingIsActive(name, members))
        return(TRUE)
      !is.function(.subset2(members, name))
    }, logical(1))]
  }
  vapply(
    names,
    function(name) {
      if (is.environment(members) && bindingIsActive(name, members))
        return(paste0("    ", name, ": active binding"))
      value <- .subset2(members, name)
      summary <- if (is.function(value))
        deparse(args(value))[[1L]]
      else if (is.null(value))
        "NULL"
      else if (is.environment(value))
        "environment"
      else if (is.atomic(value))
        paste(utils::head(as.character(value), 60L), collapse = " ")
      else
        typeof(value)
      paste0("    ", name, ": ", summary)
    },
    character(1),
    USE.NAMES = FALSE
  )
}
