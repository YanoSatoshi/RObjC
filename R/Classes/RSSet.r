#' @title RSSet: Immutable Hash-Based Set of RSObjects
#'
#' @description
#' `RSSet` is a read-only subclass of `RSMutableSet`. Once created, its elements are
#' flattened and frozen, and any attempt to modify the structure will result in an error.
#'
#' This class provides fast `containsObject()` and `count()` access, and supports copying.
#'
#' @note All mutation methods such as `addObject()` or `removeObject()` are disabled and will raise errors.
#'
#' @export
RSSet <- R6::R6Class(
  "RSSet",
  inherit = RSMutableSet,
  cloneable = FALSE,
  private = list(
    .flat = NULL,
    .freeze = function() {
      if (is.null(private$.bucket)) {
        private$.flat <- list()
      } else {
        flat <- unlist(private$.bucket, recursive = FALSE)
        private$.flat <- flat
      }
      private$.bucket <- NULL
    }
  ),
  public = list(
    initialize = function(objects = list()) {
      super$initialize(objects)
      private$.freeze()
      invisible(self)
    },
    containsObject = function(obj) {
      if (length(private$.flat) == 0) return(FALSE)
      obj_hash <- obj$hash()
      flags <- vapply(
        private$.flat,
        function(x) {x$hash() == obj_hash && x$isEqual(obj)},
        logical(1)
      )
      any(flags)
    },
    count = function() {
      length(private$.flat)
    },
    allObjectsAsList = function() {
      private$.flat
    },
    allObjects = function() {
      RSArray$new(objects = self$allObjectsAsList())
    },
    addObject = function(obj) {
      stop(msg_immutable_try_mutate(self$class()))
    },
    removeObject = function(obj) {
      stop(msg_immutable_try_mutate(self$class()))
    },
    removeAllObjects = function() {
      stop(msg_immutable_try_mutate(self$class()))
    },



#' @describeIn RSSet
#' Returns an identical immutable copy of the current object placed into the specified environment.
#' Because the object is already immutable, this method typically returns `self` unless an environment
#' switch is needed for structural consistency.
#'
#' @param env The target environment.
#' @return A copy of the current immutable object, typically `self`.
    copyWithEnv = function(env = parent.frame()) {
      if (identical(env, environment(self))) return(self)
      do.call(
        RSSet$new,
        list(objects = self$allObjectsAsList()),
        envir = env
      )
    },



#' @describeIn RSSet
#' Returns a new mutable version of the current object in the specified environment.
#' The content is duplicated to allow modifications, and the result is typically
#' an instance of the corresponding mutable subclass (e.g., `RSMutableArray` for `RSArray`).
#'
#' @param env The environment in which the new mutable object will be evaluated.
#' @return A new mutable copy of the object.
    mutableCopyWithEnv = function(env = parent.frame()) {
      do.call(
        RSMutableSet$new,
        list(objects = self$allObjectsAsList()),
        envir = env
      )
    }
  )
)
