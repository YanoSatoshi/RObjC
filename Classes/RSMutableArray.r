RSMutableArray <- R6::R6Class(
  "RSMutableArray",
  inherit = RSObject,
  cloneable = FALSE,
  private = list(
    .storage = list(),
    .hash_operator = NULL,
    .copying = NULL
  ),
  public = list(
    initialize = function(
      objects = list(),
      hash_operator = RSHashOperator$module()
    ) {
      objects <- cast_ry_storage_to_list(objects)
      stopifnot(is.list(objects))
      super$initialize()
      if (
        length(objects) > 0 &&
        !all(vapply( # nolint
          objects,
          function(obj) !is.null(obj$isKindOf) && obj$isKindOf(RSObject),
          logical(1)
        ))
      ) stop("All elements must inherit from RSObject.")
      private$.storage <- objects
      private$.hash_operator <- hash_operator
      private$.copying <- RSCopying$protocol()
      invisible(self)
    },
    addObject = function(obj) {
      stopifnot(!is.null(obj$isKindOf), obj$isKindOf(RSObject))
      copied <- private$.copying$copyObjectWithEnv(obj, environment(self))
      private$.storage <- c(private$.storage, list(copied))
      invisible(self)
    },
    insertObjectAtIndex = function(obj, index) {
      stopifnot(!is.null(obj$isKindOf), obj$isKindOf(RSObject))
      len <- length(private$.storage)
      if (index < 1 || index > len + 1) stop("Index out of bounds.")
      obj <- private$.copying$copyObjectWithEnv(obj, environment(self))
      private$.storage <-
        append(private$.storage, list(obj), after = index - 1L)
      invisible(self)
    },
    replaceObjectAtIndex = function(obj, index) {
      stopifnot(!is.null(obj$isKindOf), obj$isKindOf(RSObject))
      len <- length(private$.storage)
      if (index < 1 || index > len) stop("Index out of bounds.")
      obj <- private$.copying$copyObjectWithEnv(obj, environment(self))
      private$.storage[[index]] <- obj
      invisible(self)
    },
    removeObjectAtIndex = function(index) {
      len <- length(private$.storage)
      if (index < 1 || index > len) stop("Index out of bounds.")
      private$.storage <- private$.storage[-index]
      invisible(self)
    },
    removeAllObjects = function() {
      private$.storage <- list()
      invisible(self)
    },
    objectAtIndex = function(index) {
      len <- length(private$.storage)
      if (index < 1 || index > len) stop("Index out of bounds.")
      private$.storage[[index]]
    },
    containsObject = function(obj) {
      isTRUE(self$indexOfObject(obj))
    },
    indexOfObject = function(obj) {
      idxes <- vapply(
        private$.storage,
        function(x) x$isEqual(obj),
        logical(1)
      )
      if (any(idxes)) return(which(idxes)[1L]) else NA_integer
    },
    allObjects = function() {
      self$copyWithEnv()
    },
    asList = function() {
      lapply(private$.storage, identity)
    },
    allObjectsAsList = function() {
      self$asList()
    },
    count = function() {
      length(private$.storage)
    },
    copyWithEnv = function(env = parent.frame()) {
      do.call(
        RSArray$new,
        list(
          objects = private$.storage,
          hash_operator = private$.hash_operator
        ),
        envir = env
      )
    },
    mutableCopyWithEnv = function(env = parent.frame()) {
      if (identical(env, environment(self))) return(self)
      do.call(
        RSMutableArray$new,
        list(
          objects = private$.storage,
          hash_operator = private$.hash_operator
        ),
        envir = env
      )
    },
    hash = function() {
      self$hashOperator()$hashFromObjects(private$.storage)
    },
    hashesOfObjects = function() {
      self$hashOperator()$hashesOfEachObject(private$.storage)
    },
    hashOperator = function() {
      private$.hash_operator
    },
    description = function() {
      sprintf("<%s: %d objects>", self$class()$classname, self$count())
    },
    print = function(...) {
      cat(self$description(), "\n")
      invisible(self)
    },
    isEqual = function(other) {
      if (!other$isMemberOf(self$class())) return(FALSE)
      other_objs <- other$asList()
      if (length(other_objs) != self$count()) return(FALSE)
      all(
        mapply(
          function(x, y) x$isEqual(y),
          private$.storage,
          other_objs,
          USE.NAMES = FALSE
        )
      )
    },
    compare = function(other) {
      if (!self$isMemberOf(other$class())) {
        stop("Cannot compare objects of different class.")
      }
      h1 <- self$hash()
      h2 <- other$hash()
      if (is.na(h1) || is.na(h2)) return(NA_integer_)
      if (h1 < h2) return(-1L)
      if (h1 > h2) return(1L)
      return(0L)
    }
  )
)
