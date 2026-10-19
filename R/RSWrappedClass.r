#' @include RSObject.r
#' @title RSWrappedClass: Immutable Identifier for Class Definitions
#'
#' @description
#' `RSWrappedClass` is an immutable wrapper that uniquely identifies a class definition
#' using a combination of the class name and version string.
#' This identifier, stored as an `RSString` in `class_id`, serves as a stable reference
#' for use in serialization maps, class registries, and instance tracking structures.
#'
#' The wrapper holds a reference to the class generator (`classObject()`) and generates
#' the identifier in the format `<ClassName>_<Version>`. The version string must not contain underscores.
#' The version comes from the generator's `robjc_isa$version` field.
#'
#' Once created, its `class_ref` and `class_id` bindings are locked.
#' Equality and hashing use `class_id`, so separate generators with the same
#' name and version have equal wrappers.
#'
#' @section Inheritance:
#' Inherits from [RSObject].
#'
#' @section Fields:
#' - `class_ref`: The class generator object being wrapped.
#' - `class_id`: An [RSString] representing the combined name and version.
#'
#' @section Methods:
#' - `classObject()`: Returns the R6 class generator object (e.g., `MyClass`).
#' - `classID()`: Returns the class identifier as an [RSString].
#' - `className()`: Returns the class name as a character string.
#' - `version()`: Returns the version portion of the identifier string.
#'
#' @section Constraints:
#' - Version strings must not contain underscores.
#' - The input must be an RObjC object class generator or an RObjC instance.
#'
#' @seealso [RSClassReference], [RSInstanceMap], [RSObject]
#'
#' @examples
#' \dontrun{
#' wrapped <- RSWrappedClass$new(obj)
#' wrapped$classID()      # => RSString: "MyClass_1.0"
#' wrapped$className()    # => "MyClass"
#' wrapped$version()      # => "1.0"
#' }
#'
#' @export
RSWrappedClass <- RSObjectClass(
  "RSWrappedClass",
  version = "0.0.3 alpha",
  super_class = RSObject,
  public = list(

    #' @field class_ref
    #'   The class object being wrapped. This is an R6 class generator, typically defined by the user.
    #'   It must inherit from `RSObject`. Used for accessing metadata or generating new instances.
    class_ref = NULL,



    #' @field class_id [RSString]
    #'   A string identifier uniquely representing the class. It is constructed as `<ClassName>_<Version>`.
    #'   The version comes from the class generator's `robjc_isa$version` field.
    #'   The underscore character is reserved as a separator and must not appear in version strings.
    class_id  = NULL,



    #' @description
    #' Initializes a new wrapped class object based on the given RSObject instance.
    #' The wrapper records the class generator and constructs a unique identifier
    #' from the class name and version.
    #'
    #' @param object An RObjC object or its class generator.
    #'
    #' @return The locked `RSWrappedClass` object (invisibly).
    #'
    #' @section Constraints:
    #' - The version string must not contain an underscore ("_").
    #' - The version is read from the generator's `robjc_isa$version` field.
    #'
    #' @examples
    #' \dontrun{
    #' wrapped <- RSWrappedClass$new(obj)
    #' }
    initialize = function(object) {
      super$initialize()
      if (robjc_isClass(object))
        klass <- object
      else if (robjc_isRobjcObject(object))
        klass <- object$class()
      else
        stop("`object` must be an RObjC class or instance.", call. = FALSE)
      version <- klass$robjc_isa$version
      stopifnot("Do not contin '_' for version expression." = !grepl("_", version))
      self$class_id <- RSString_withValue(
        sprintf("%s_%s", klass$classname, version)
      )
      lockBinding("class_id",  self)
      self$class_ref <- klass
      lockBinding("class_ref", self)
      invisible(self)
    },



    #' @description Compare class identifiers by class name and version.
    #' @param other Another `RSWrappedClass` object.
    #' @return `TRUE` if both wrappers have the same class ID, otherwise `FALSE`.
    isEqual = function(other) {
      robjc_isRobjcObject(other) &&
        other$isKindOf(RSWrappedClass) &&
        isTRUE(self$classID()$isEqual(other$classID()))
    },



    #' @description Hash the class identifier.
    #' @return The class ID's integer hash.
    hash = function() self$classID()$hash(),



    #' @description
    #' Returns the underlying class generator object associated with the wrapped class.
    #'
    #' @return The R6 class generator (as returned by \code{obj$class()}).
    #'
    #' @examples
    #' \dontrun{
    #' gen <- wrapped$classObject()
    #' }
    classObject = function() self$class_ref,



    #' @description
    #' Returns the full class identifier string in the form `<ClassName>_<Version>`.
    #' This value is returned as an [RSString] and can be used for keying in maps or registries.
    #' The value is immutable once the object is initialized.
    #'
    #' @return An [RSString] object representing the class identifier.
    #'
    #' @examples
    #' \dontrun{
    #' wrapped$classID()  # => RSString: "MyClass_1.0"
    #' }
    classID = function() self$class_id,



    #' @description
    #' Returns the class name string of the wrapped class.
    #'
    #' @return A character string (e.g., `"MyClass"`).
    #'
    #' @examples
    #' \dontrun{
    #' wrapped$className()  # "RSUser"
    #' }
    className = function() self$class_ref$classname,



    #' @description
    #' Returns the version string extracted from the class ID.
    #'
    #' @return A character string representing the version (e.g., `"1.0"`).
    #'
    #' @examples
    #' \dontrun{
    #' wrapped$version()  # "1.0"
    #' }
    version   = function() sub(".*_", "", self$class_id$value())
  )
)
