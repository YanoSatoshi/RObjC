#' @title RSClassReference: Reference from Class ID to Instance Map
#'
#' @description
#' `RSClassReference` acts as a reference entry in a serialization context (e.g., inside
#' an `RSClassEncoder`) that maps a unique class ID string to a corresponding `RSInstanceMap`.
#'
#' The key is a string identifier such as `"MyClass_v1"`, and the value is an `RSInstanceMap`
#' that manages all known instances of that class.
#'
#' This class assumes that the wrapped class conforms to the `RSEncoding` protocol;
#' actual validation is delegated to the `RSInstanceMap` created during initialization.
#'
#' @section Inheritance:
#' Inherits from [RSKeyValuePair], which enforces that the key conforms to the [RSCopying] protocol.
#' - the key is a character string
#' - the value is an [RSInstanceMap]
#'
#' @section Fields:
#' - `classID()`: Returns the unique class ID string.
#' - `classRef()`: Returns the associated `RSWrappedClass` (via instance map).
#' - `instanceMap()`: Returns the instance map containing the object set.
#'
#' @seealso [RSInstanceMap], [RSWrappedClass], [RSEncoding]
#'
#' @examples
#' \dontrun{
#' ref <- RSClassReference$new(my_wrapped_class, list(inst1, inst2))
#' ref$classID()         # "MyClass_v1"
#' ref$classRef()        # RSWrappedClass
#' ref$instanceMap()$objects()$count()  # => 2
#' }
#'
#' @keywords internal
RSClassReference <- R6::R6Class(
  "RSClassReference",
  inherit = RSKeyValuePair,
  cloneable = FALSE,
  public = list(

    #' @description
    #' Initializes a new `RSClassReference` object by associating a wrapped class definition
    #' with a collection of instances. This method constructs an `RSInstanceMap` internally,
    #' sets the class identifier as the key, and locks the reference object to make it immutable.
    #'
    #' The input `objects` can be a plain list of instances or a robjc-compatible container
    #' (such as `RSMutableSet`) that responds to `asList()`. The internal instance map always
    #' stores the instances as an `RSMutableSet`.
    #'
    #' @param class_ref An object of class `RSWrappedClass`, representing the class definition.
    #' @param objects A list or compatible robjc container of instances that belong to `class_ref`.
    #'
    #' @return A new, locked `RSClassReference` object.
    #'
    #' @examples
    #' \dontrun{
    #' ref <- RSClassReference$new(my_wrapped_class, list(instance1, instance2))
    #' ref$classID()        # "MyClass_v1"
    #' ref$instanceMap()    # RSInstanceMap
    #' }
    #'
    #' @section Validation:
    #' This method validates that \code{class_ref} is an instance of `RSWrappedClass`.
    #'
    #' @section Locking:
    #' The resulting object is locked using \code{lockObject(self)} to ensure structural immutability.
    initialize = function(class_ref, objects = list()) {
      stopifnot("Class_ref is not kind of RSWrappedClass." =
        class_ref$isKindOf(RSWrappedClass)
      )
      super$initialize(
        class_ref$class_id,
        RSInstanceMap$new(class_ref, cast_rs_storage_to_list(objects))
      )
      lockBinding(".key",   private)
      lockBinding(".value", private)
      invisible(self)
    },



    #' @description
    #' Returns the unique class identifier string used as the key for this class reference.
    #' The value typically includes the class name and version (e.g., "MyClass_v1").
    #'
    #' @return A character string representing the class ID.
    #'
    #' @examples
    #' \dontrun{
    #' ref$classID()  # "MyClass_v1"
    #' }
    classID = function() private$.key,


    #' @description
    #' Returns the `RSWrappedClass` object associated with this class reference.
    #' This is equivalent to \code{ref$instanceMap()$wrappedClass()} and provides direct
    #' access to the underlying class definition.
    #'
    #' @return An object of class `RSWrappedClass`.
    #'
    #' @examples
    #' \dontrun{
    #' wrapped_cls <- ref$classRef()
    #' wrapped_cls$classname()  # "MyClass"
    #' }
    classRef = function()private$.value$wrappedClass(),



    #' @description
    #' Returns the `RSInstanceMap` object that holds the instances for this class.
    #' The map contains a reference to the wrapped class and a mutable set of instances.
    #'
    #' @return An object of class `RSInstanceMap`.
    #'
    #' @examples
    #' \dontrun{
    #' map <- ref$instanceMap()
    #' map$objects()$count()  # number of instances
    #' }
    instanceMap = function() private$.value
  )
)
