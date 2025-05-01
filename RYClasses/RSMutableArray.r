RSMutableArray <- R6::R6Class(
  "RSMutableArray",
  inherit = RSObject,
  cloneable = FALSE,
  public = list(
    initialize = function(objects = list(), hash_operator = RSHashOperator$module()) {
      stopifnot(is.list(objects))
      super$initialize()
      if (
        length(objects) > 0 &&
        !all(vapply(
          objects,
          function(obj) {!is.null(obj$isKindOf) && obj$isKindOf(RSObject)},
          logical(1)
        ))
      ) stop("All elements must inherit from RSObject.")
      private$.storage <- objects
      private$.hash_operator <- hash_operator
      invisible(self)
    },
    addObject = function(obj) {
      stopifnot(!is.null(obj$isKindOf), obj$isKindOf(RSObject))
      private$.storage <- c(private$.storage, list(obj))
      invisible(self)
    },
    insertObjectAtIndex = function(obj, index) {
      stopifnot(!is.null(obj$isKindOf), obj$isKindOf(RSObject))
      len <- length(private$.storage)
      if (index < 1 || index > len + 1) stop("Index out of bounds.")
      private$.storage <- append(private$.storage, list(obj), after = index - 1L)
      invisible(self)
    },
    removeObjectAtIndex = function(index) {
      len <- length(private$.storage)
      if (index < 1 || index > len) stop("Index out of bounds.")
      private$.storage <- private$.storage[-index]
      invisible(self)
    },
    objectAtIndex = function(index) {
      len <- length(private$.storage)
      if (index < 1 || index > len) stop("Index out of bounds.")
      private$.storage[[index]]
    },
    replaceObjectAtIndex = function(index, obj) {
      stopifnot(!is.null(obj$isKindOf), obj$isKindOf(RSObject))
      len <- length(private$.storage)
      if (index < 1 || index > len) stop("Index out of bounds.")
      private$.storage[[index]] <- obj
      invisible(self)
    },
    allObjects = function() {
      private$.storage
    },
    count = function() {
      length(private$.storage)
    },
    clear = function() {
      private$.storage <- list()
      invisible(self)
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
    }
  ),
  private = list(
    .storage = list(),
    .hash_operator = NULL
  )
)
