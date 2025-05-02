RSMutableSet <- R6::R6Class(
  "RSMutableSet",
  inherit = RSDynamicHash,
  cloneable = FALSE,
  private = list(
    .copying = NULL
  ),
  public = list(
    initialize = function(objects = list()) {
      objects <- cast_ry_storage_to_list(objects)
      if (
        length(objects) > 0 &&
        !all(vapply(
          objects,
          function(obj) !is.null(obj$isKindOf) && obj$isKindOf(RSObject),
          logical(1)
        ))
      ) stop("All elements must inherit from RSObject.")
      super$initialize(objects = objects)
      private$.copying <- RSCopying$protocol()
      invisible(self)
    },
    addObject = function(obj) {
      stopifnot(!is.null(obj$isKindOf), obj$isKindOf(RSObject))
      copied <- private$.copying$copyObjectWithEnv(obj, environment(self))
      super$setObject(copied)
    },
    containsObject = function(obj) {
      !is.null(super$getObject(obj))
    },
    allObjectsAsList = function() {
      super$allObjectsAsList()
    },
    allObjects = function() {
      RSArray$new(self$allObjectsAsList())
    },
    unionSet = function(other) {
      stopifnot(other$isKindOf(RSMutableSet))
      union <- self$hash_operator$unionOfSets(
        self$allObjectsAsList(), other$allObjectsAsList()
      )
      RSSet$new(union)
    },
    intersectSet = function(other) {
      stopifnot(other$isKindOf(RSMutableSet))
      intersection <- self$hash_operator$intersectionOfSets(
        self$allObjectsAsList(),
        other$allObjectsAsList()
      )
      RSSet$new(intersection)
    },
    differenceSet = function(other) {
      stopifnot(other$isKindOf(RSMutableSet))
      difference <- self$hash_operator$differenceOfSets(
        self$allObjectsAsList(),
        other$allObjectsAsList()
      )
      RSSet$new(difference)
    },
    copyWithEnv = function(env = parent.frame()) {
      do.call(
        RSSet$new,
        list(objects = self$allObjectsAsList()),
        envir = env
      )
    },
    mutableCopyWithEnv = function(env = parent.frame()) {
      if (identical(env, environment(self))) return(self)
      do.call(
        RSMutableSet$new,
        list(objects = self$allObjectsAsList()),
        envir = env
      )
    }
  )
)
