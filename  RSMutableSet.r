RSMutableSet <- R6::R6Class(
  "RSMutableSet",
  inherit = RSDynamicHash,
  cloneable = FALSE,
  public = list(
    initialize = function(objects = list()) {
      super$initialize()
      for (obj in objects) {
        stopifnot(inherits(obj, "RSObject"))
        super$setObject(obj)
      }
      invisible(self)
    },
    addObject = function(obj) {
      super$setObject(obj)
    },
    removeObject = function(obj) {
      super$removeObject(obj)
    },
    clear = function() {
      private$.initializeTable()
      invisible(self)
    },
    containsObject = function(obj) {
      !is.null(super$getObject(obj))
    },
    unionSet = function(other) {
      stopifnot(other$isKindOf(RSMutableSet))
      union <- self$hash_operator$unionOfSets(
        self$allObjects(), other$allObjects()
      )
      RSSet$new(union)
    },
    intersectSet = function(other) {
      stopifnot(other$isKindOf(RSMutableSet))
      intersection <- self$hash_operator$intersectionOfSets(
        self$allObjects(), other$allObjects()
      )
      RSSet$new(intersection)
    },
    differenceSet = function(other) {
      stopifnot(other$isKindOf(RSMutableSet))
      difference <- self$hash_operator$differenceOfSets(
        self$allObjects(), other$allObjects()
      )
      RSSet$new(difference)
    }
  ),
  private = list(
    .initializeTable = function() {
      private$.bucket[] <- rep(list(NULL), length(private$.bucket))
      private$.count <- 0
    }
  )
)
