DummyClass <- RSObjectClass("DummyClass", version = "1.2")

DefaultVersionClass <- RSObjectClass("DefaultVersionClass")

NestedDummy <- RSObjectClass(
  "NestedDummy",
  version = "1.0",
  public = list(
    initialize = function(child) {
      stopifnot(robjc_isRobjcObject(child))
      super$initialize()
      private$.child <- child
      invisible(self)
    },
    objectsForEncoding = function() list(private$.child)
  ),
  private = list(.child = NULL)
)
