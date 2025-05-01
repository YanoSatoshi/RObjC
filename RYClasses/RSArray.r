RSArray <- R6::R6Class(
  "RSArray",
  inherit = RSObject,
  cloneable = FALSE,
  public = list(
    initialize = function(objects = list()) {
      super$initialize()
      stopifnot(is.list(objects))
      if (length(objects) > 0) {
        valid <- vapply(objects, function(obj) {
          !is.null(obj$isKindOf) && obj$isKindOf(RSObject)
        }, logical(1))
        if (!all(valid)) stop("All elements must inherit from RSObject.")
      }
      private$.storage <- objects
      invisible(self)
    },
    objectAtIndex = function(index) {
      len <- length(private$.storage)
      if (index < 1 || index > len) stop("Index out of bounds.")
      private$.storage[[index]]
    },
    allObjects = function() {
      private$.storage
    },
    count = function() {
      length(private$.storage)
    },
    containsObject = function(obj) {
      isTRUE(self$indexOfObject(obj))
    },
    indexOfObject = function(obj) {
      idxes <- vapply(function(x) x$isEqual(obj), logical(1))
      if (any(idexes)) return(which(idxes)[1L]) else NA_integer
    },
    hash = function() {
      RSHashOperator$module()$hashFromObjects(private$.storage)
    },
    isEqual = function(other) {
      if (!other$isKindOf(RSArray)) return(FALSE)
      other_objs <- other$allObjects()
      if (length(other_objs) != self$count()) return(FALSE)
      all(mapply(function(x, y) x$isEqual(y), private$.storage, other_objs, USE.NAMES = FALSE))
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
    .storage = list()
  )
)
