#' @include RSKeyValuePair.r
#' @title RSInstanceMap: Mapping from Wrapped Class to Instance Set
#'
#' @description
#' `RSInstanceMap` represents a one-to-many mapping between a wrapped class
#' (an object of class `RSWrappedClass`) and a mutable or immutable set of instance objects.
#' It is used internally in robjc serialization to track which objects were created
#' from a given class, supporting reconstruction and encoding workflows.
#'
#' Initialization stores the provided wrapped class and places the instances in
#' an [RSMutableSet].
#'
#' @section Inheritance:
#' Inherits from [RSKeyValuePair], where:
#' - the key is an `RSWrappedClass`
#' - the value is an `RSMutableSet` (converted to `RSSet` after finalization)
#'
#' @section Lifecycle:
#' Use `$lockInstance()` to convert the map to an immutable state. Once finalized, the set of
#' instances becomes locked and cannot be modified.
#'
#' @family robjc serialization
#'
#' @seealso [RSWrappedClass], [RSEncoding], [RSMutableSet], [RSSet]
#'
#' @examples
#' \dontrun{
#' map <- RSInstanceMap$new(my_wrapped_class, list(obj1, obj2))
#' map$objects()$count()  # => 2
#' map$lockInstance()
#' }
#'
#' @keywords internal
RSInstanceMap <- RSObjectClass(
  "RSInstanceMap",
  version = "0.0.3 alpha",
  super_class = RSKeyValuePair,
  public = list(

    #' @description
    #' Initializes a new `RSInstanceMap` with the given class definition and an optional list of instances.
    #' The instance list is wrapped in an `RSMutableSet`.
    #'
    #' @param class_ref An object of class `RSWrappedClass`.
    #' @param objects A list of instance objects (optional). Defaults to an empty list.
    #'
    #' @return A new `RSInstanceMap` object.
    #'
    #' @examples
    #' \dontrun{
    #' map <- RSInstanceMap$new(my_wrapped_class, list(obj1, obj2))
    #' }
    initialize = function(class_ref, objects = list()) {
      super$initialize(class_ref, RSMutableSet$new(objects))
      private$.locked <- FALSE
      invisible(self)
    },



    #' @description
    #' Returns the identifier of the wrapped class, in the format
    #' `ClassName_Version`.
    #'
    #' @return An [RSString] containing the class ID.
    #'
    #' @examples
    #' \dontrun{
    #' map$classID()$value()  # "MyClass_1.0"
    #' }
    classID      = function() private$.key$classID(),



    #' @description
    #' Returns the `RSWrappedClass` object associated with this instance map.
    #'
    #' @return An object of class `RSWrappedClass`.
    #'
    #' @examples
    #' \dontrun{
    #' cls <- map$wrappedClass()
    #' cls$className()  # "MyClass"
    #' }
    wrappedClass = function() private$.key,



    #' @description
    #' Returns the current set of instance objects associated with the class.
    #' This may be a mutable set (`RSMutableSet`) before finalization,
    #' or an immutable set (`RSSet`) after finalization.
    #'
    #' @return An `RSSet` or `RSMutableSet` object.
    #'
    #' @examples
    #' \dontrun{
    #' objs <- map$objects()
    #' objs$count()
    #' }
    objects      = function() private$.value,



    #' @description
    #' Adds a new instance object to the set of associated instances.
    #' This method is only valid before the map has been finalized.
    #'
    #' @param obj An instance object to be added.
    #'
    #' @return The instance map itself (invisibly).
    #'
    #' @examples
    #' \dontrun{
    #' map$addObject(instance3)
    #' }
    addObject    = function(obj) {
      stopifnot("RSInstanceMap cannot modify after finalize." = !private$.locked)
      private$.value$addObject(obj)
    },



    #' @description
    #' Locks the instance map by converting its internal storage from a mutable
    #' [RSMutableSet] to an immutable [RSSet], and making the map itself read-only.
    #' After this method is called, no further instances can be added to the map.
    #'
    #' @return The locked instance map, invisibly.
    #'
    #' @note
    #' This method should be called exactly once. A second call will result in an warning
    #' due to locked bindings.
    #'
    #' @examples
    #' \dontrun{
    #' map$lockInstance()
    #' map$objects()$addObject(obj)  # Error: set is locked
    #' }
    lockInstance = function() {
      if (private$.locked) {
        warning("RSInstanceMap$lockInstance() was called again and skipped.")
        return(invisible(self))
      }
      contents <- private$.value$allObjectsAsList()
      private$.value <- RSSet$new(contents)
      private$.locked <- TRUE
      lockBinding(".key",   private)
      lockBinding(".value", private)
      lockBinding(".locked", private)
    }
  ),
  private = list(
    .locked = FALSE
  )
)
