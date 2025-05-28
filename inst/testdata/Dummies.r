DummyClass <- R6::R6Class("DummyClass", inherit = RSObject,
  public = list(
    initialize = function() {
      uninit <- is.null(private$.__isa)
      super$initialize(.version = "1.2")
      invisible(self)
    }
  )
)

NoVersionClass <- R6::R6Class("NoVersionClass", inherit = RSObject)

BadVersionClass <- R6::R6Class("BadVersionClass", inherit = RSObject,
  public = list(
    initialize = function() {
      super$initialize(.version = "1_0")
      invisible(self)
    }
  )
)


NestedDummy <- R6::R6Class("NestedDummy", inherit = RSObject,
  public = list(
    initialize = function(child) {
      stopifnot(is_robjc_instance(child))
      super$initialize(.version = "1.0")
      private$.child <- child
      invisible(self)
    },
    objectsForEncoding = function() list(private$.child)
  ),
  # resolveClassWith = function(){},
  private = list(.child = NULL)
)