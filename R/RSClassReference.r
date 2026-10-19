#' @include RSKeyValuePair.r
#'
#' @title RSClassReference: Reference from Class ID to Instance Map
#'
#' @description
#' `RSClassReference` acts as a reference entry in a serialization context (e.g., inside
#' an `RSClassEncoder`) that maps a unique class ID string to a corresponding `RSInstanceMap`.
#'
#' The key is an [RSString] identifier such as `"MyClass_1.0"`, and the value is an `RSInstanceMap`
#' that manages all known instances of that class.
#'
#' Initialization validates that `class_ref` is an [RSWrappedClass] instance.
#'
#' @section Inheritance:
#' Inherits from [RSKeyValuePair]. `RSClassReference` uses an [RSString]
#' class identifier as its key.
#' - the key is an [RSString]
#' - the value is an [RSInstanceMap]
#'
#' @section Fields:
#' - `classID()`: Returns the unique class ID as an [RSString].
#' - `classRef()`: Returns the associated `RSWrappedClass` (via instance map).
#' - `instanceMap()`: Returns the instance map containing the object set.
#'
#' @seealso [RSInstanceMap], [RSWrappedClass], [RSEncoding]
#'
#' @examples
#' \dontrun{
#' ref <- RSClassReference$new(my_wrapped_class, list(inst1, inst2))
#' ref$classID()$value() # "MyClass_1.0"
#' ref$classRef()        # RSWrappedClass
#' ref$instanceMap()$objects()$count()  # => 2
#' }
#'
#' @keywords internal
RSClassReference <- RSObjectClass(
  "RSClassReference",
  version = "0.0.3 alpha",
  super_class = RSKeyValuePair,
  public = list(

    #' @description
    #' Initializes a new `RSClassReference` object by associating a wrapped class definition
    #' with a collection of instances. This method constructs an `RSInstanceMap` internally,
    #' sets the class identifier as the key, and locks the private key and value bindings.
    #'
    #' The input `objects` can be a plain list of instances or a robjc-compatible container
    #' (such as `RSMutableSet`) that responds to `asList()`. The internal instance map always
    #' stores the instances as an `RSMutableSet`.
    #'
    #' @param class_ref An object of class `RSWrappedClass`, representing the class definition.
    #' @param objects A list or compatible robjc container of instances that belong to `class_ref`.
    #'
    #' @return A new `RSClassReference` object.
    #'
    #' @examples
    #' \dontrun{
    #' ref <- RSClassReference$new(my_wrapped_class, list(instance1, instance2))
    #' ref$classID()$value() # "MyClass_1.0"
    #' ref$instanceMap()    # RSInstanceMap
    #' }
    #'
    #' @section Validation:
    #' This method validates that \code{class_ref} is an instance of `RSWrappedClass`.
    #'
    #' @section Locking:
    #' The private key and value bindings are locked after initialization.
    initialize = function(class_ref, objects = list()) {
      if (!robjc_isRobjcObject(class_ref) || !class_ref$isKindOf(RSWrappedClass))
        stop("`class_ref` must be an RSWrappedClass object.", call. = FALSE)
      super$initialize(
        class_ref$class_id,
        RSInstanceMap$new(class_ref, cast_collections_to_list(objects))
      )
      lockBinding(".key",   private)
      lockBinding(".value", private)
      invisible(self)
    },



    #' @description
    #' Returns the class identifier stored as the key for this reference.
    #' It includes the class name and version (e.g., "MyClass_1.0").
    #'
    #' @return An [RSString] containing the class ID.
    #'
    #' @examples
    #' \dontrun{
    #' ref$classID()$value()  # "MyClass_1.0"
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
    #' wrapped_cls$className()  # "MyClass"
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
