#' @include RSKeyValuePair.r
#' @title RSInstanceMap: Mapping from Wrapped Class to Instance Set
#'
#' @description
#' `RSInstanceMap` represents a one-to-many mapping between a wrapped class
#' (an object of class `RSWrappedClass`) and a set of instance objects.
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
#' Use `$lockInstance()` to convert the map to an immutable state. Before then,
#' add instances through `$addObject()`; `$objects()` returns an immutable snapshot.
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
  version = "0.0.4 alpha",
  super_class = RSKeyValuePair,
  public = list(

    #' @description
    #' Initializes a new `RSInstanceMap` with the given class definition and an optional list of instances.
    #' The instance list is wrapped in an `RSMutableSet`.
    #'
    #' @param class_ref An object of class `RSWrappedClass`.
    #' @param objects A list of instances with the wrapped class ID (optional).
    #'   Defaults to an empty list.
    #'
    #' @return A new `RSInstanceMap` object.
    #'
    #' @examples
    #' \dontrun{
    #' map <- RSInstanceMap$new(my_wrapped_class, list(obj1, obj2))
    #' }
    initialize = function(class_ref, objects = list()) {
      if (!robjc_isRobjcObject(class_ref) || !class_ref$isKindOf(RSWrappedClass))
        stop("`class_ref` must be an RSWrappedClass object.", call. = FALSE)
      objects <- cast_collections_to_list(objects)
      valid <- vapply(
        objects,
        function(obj) private$.matchesClassID(obj, class_ref),
        logical(1)
      )
      if (!all(valid))
        stop("Instances must belong to `class_ref`.", call. = FALSE)
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
    #' Returns an immutable snapshot of the current instance set.
    #'
    #' @return An `RSSet` containing the current instances.
    #'
    #' @examples
    #' \dontrun{
    #' objs <- map$objects()
    #' objs$count()
    #' }
    objects      = function() private$.value$copy(),



    #' @description Returns the same immutable snapshot as `objects()`.
    #' @return An `RSSet` containing the current instances.
    value = function() self$objects(),



    #' @description Returns the wrapped class and an immutable instance snapshot
    #'   for encoding.
    #' @return A list containing the wrapped class and an `RSSet` snapshot.
    objectsForEncoding = function() list(private$.key, self$objects()),



    #' @description
    #' Adds a new instance object to the set of associated instances.
    #' This method is only valid before the map has been finalized.
    #'
    #' @param obj An instance with the wrapped class ID to be added.
    #'
    #' @return The instance map itself (invisibly).
    #'
    #' @examples
    #' \dontrun{
    #' map$addObject(instance3)
    #' }
    addObject    = function(obj) {
      stopifnot("RSInstanceMap cannot modify after finalize." = !private$.locked)
      if (!private$.matchesClassID(obj, private$.key))
        stop("Instance must belong to `class_ref`.", call. = FALSE)
      private$.value$addObject(obj)
      invisible(self)
    },



    #' @description
    #' Locks the instance map by converting its internal storage from a mutable
    #' [RSMutableSet] to an immutable [RSSet], and making the map itself read-only.
    #' After this method is called, no further instances can be added to the map.
    #'
    #' @return The locked instance map, invisibly.
    #'
    #' @note
    #' Repeated calls leave the locked map unchanged and issue a warning.
    #'
    #' @examples
    #' \dontrun{
    #' map$lockInstance()
    #' map$addObject(obj)  # Error: map is locked
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
      invisible(self)
    }
  ),
  private = list(
    .locked = FALSE,
    .matchesClassID = function(obj, class_ref) {
      if (!robjc_isRobjcObject(obj)) return(FALSE)
      klass <- obj$class()
      identical(
        sprintf("%s_%s", klass$classname, klass$robjc_isa$version),
        class_ref$classID()$value()
      )
    }
  )
)
