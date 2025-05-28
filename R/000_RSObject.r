#' @importFrom R6 R6Class

#' @title RSObject: A Fundamental Base Class Inspired by Objective-C's NSObject
#'
#' @description
#' `RSObject` is the fundamental base class for all RS-based classes,
#' designed to emulate the behavior and philosophy of Objective-C's `NSObject`.
#' It provides basic object-oriented functionalities such as dynamic method response checks,
#' protocol conformance, class identity, hash generation, and human-readable descriptions.
#'
#' This class should be inherited by other classes in the RS framework and is not
#' intended to be used directly by end users. It supports type checking via `isMemberOf()` and `isKindOf()`,
#' enabling class-based introspection and inheritance-aware comparisons.
#'
#' @section Public Methods:
#' - `initialize()` – Internal constructor that assigns class metadata.
#' - `class()` – Returns the internal class object of the instance.
#' - `conformsTo(protocol)` – Returns `TRUE` if the object conforms to the specified protocol.
#' - `respondsTo(method)` – Returns `TRUE` if the object implements the specified method.
#' - `description()` – Returns a concise string representation of the object.
#' - `debugDescription()` – Returns a detailed string including all public fields and their structure.
#' - `print()` – Outputs the result of `description()` to the console.
#' - `isMemberOf(cls)` – Checks if the object is a direct instance of the given class.
#' - `isKindOf(cls)` – Checks if the object inherits from the given class (directly or indirectly).
#' - `isEqual(other)` – Returns `TRUE` if the reference is identical to `other`.
#' - `hash()` – Computes a numeric hash based on the memory address of the object.
#' - `classVersion()` – Returns the class object version such as "1.0.0"
#' - `objectID()` - Returns the pointer of instance.
#'
#' @section Internal Fields:
#' - `.__isa` – Cached class object.
#'
#' @section Notes:
#' - The hash method extracts a portion of the object's environment pointer as a unique key.
#' - Class identity is preserved via explicit references, not via string-based comparisons.
#'
#' @examples
#' obj <- RSObject$new()
#' obj$description()
#' obj$respondsTo("hash")
#' obj$isMemberOf(RSObject)
#'
#' @export
RSObject <- R6::R6Class(
  "RSObject",
  cloneable = FALSE,
  public = list(


    #' @description
    #' Initializes a new instance of an `RSObject` subclass.
    #'
    #' During initialization, this method sets up the internal `robjc_isa` metadata
    #' for the class if it has not already been initialized. This includes establishing:
    #' 
    #' - The class version (`version`)
    #' - The full inheritance chain (`inherit_chain`)
    #' - The immediate superclass (`super_class`)
    #' - The list of adopted protocols (`protocols`)
    #' - A general-purpose slot (`slot`)
    #'
    #' If the class has already been initialized, this step is skipped to avoid redefinition.
    #'
    #' The `.version` argument allows the developer to explicitly set the version string
    #' associated with the class. If omitted, it defaults to `"0.0"`. This version is used
    #' for compatibility checks during object deserialization.
    #'
    #' @param .version Optional character or numeric version identifier for the class.
    #'   If `NULL`, defaults to `"0.0"`.
    #'
    #' @return The initialized object itself (invisibly).
    #'
    #' @keywords internal
    initialize = function(.version = NULL) {
      # get Class Object
      class_name <- class(self)[1]  # Only here, get classname from func class().  # nolint
      base_env   <- environment(self$initialize)
      search_env <- NULL
      if (identical(base_env, .GlobalEnv)) {
        search_env <- parent.env(base_env)
      } else {
        search_env <- base_env
      }
      private$.__isa <- get(class_name, envir = search_env, inherits = TRUE)
      if (is.null(private$.__isa$robjc_isa)) {
        private$.__isa$robjc_isa <- new.env(parent = emptyenv())
        ## inside robjc_isa ##
        #   char version;       Version string
        #   list inherit_chain; List starts with class of itself and ends with RSObject.
        #                       [CurrentClass, SuperClass, .., RSObject]
        #   Class super         Super class.
        #   list protocols;     List of declarating protocol adoptation.
        #                       [SomeProtocol, .., RSCopying, RSEncoding]
        #   var  slot;          A slot. In singleton pattern, the slot stores
        #                       singleton instance.
        ### end ###
        isa <- private$.__isa$robjc_isa
        # class version setting
        isa$version <- if (is.null(.version)) "0.0" else as.character(.version)
        # class-chain setting
        isa$inherit_chain <- resolve_class_ancestors(private$.__isa)
        # super class
        isa$super_class <- isa$inherit_chain[[2]]
        # delegate protocol setting
        isa$protocols <- resolve_protocol_ancestors(private$.__isa)
        # slot
        isa$slot <- NULL
      }
      invisible(self)
    },



    #' @description
    #' Returns the internal class object.
    #' @return The object's class definition.
    class = function() private$.__isa,



    #' @description
    #' Returns the super-class object.
    #' @return The object's super-class definition.
    superClass = function() self$class()$robjc_isa$super_class,



    #' @description
    #' Checks if object conforms to a given protocol.
    #' @param protocol A character string naming the protocol.
    #' @return TRUE if conforms, FALSE otherwise.
    conformsTo = function(protocol) {
      cls <- self$class()
      if (is.null(cls$robjc_isa$protocols)) return(FALSE)
      if (is.function(protocol$class)) protocol <- protocol$class()
      if (!is.character(protocol)) protocol <- protocol$classname
      protocol %in% names(cls$robjc_isa$protocols)
    },



    #' @description
    #' Checks if object responds to a method.
    #' @param method A character string for the method name.
    #' @return TRUE if the method exists and is callable.
    respondsTo = function(method) {
      in_public <-
        exists(method, where = self, inherits = TRUE) &&
        is.function(self[[method]])
      in_private <-
        exists(method, where = private, inherits = TRUE) &&
        is.function(private[[method]])
      in_public || in_private
    },



    #' @description
    #' Returns a compact description like "<Class 0xabc123>".
    #' @return A character string describing the object.
    description = function() {
      paste0("<", self$class()$classname, " ", get_pointer_hex(self), ">")
    },



    #' @description
    #' Returns debug-level string with public field summary.
    #' @return A character string.
    debugDescription = function() {
      descript <- self$description()
      fields <- ls(self, all.names = TRUE)
      if (length(fields) == 0) return(paste0(descript, "{ }"))
      field_values <- vapply(fields,
        function(f){
          value <- tryCatch(self[[f]], error = function(e){"<error>"})
          val_str <- capture.output(str(value, max.level = 1))
          paste(val_str, collapse = " ")
        },
        character(1)
      )
      fields_info <- paste(fields, ": ", field_values, collapse =", ")
      paste0(descript, "{", fields_info, "}")
    },



    #' @description
    #' Prints the object description.
    #' @return Invisible self.
    print = function() {
      cat(self$description(), "\n")
      invisible(self)
    },



    #' @description
    #' Checks if object is an exact instance of class `cls`.
    #' @param cls A class object.
    #' @return TRUE if the class matches exactly.
    isMemberOf = function(cls) {
      my_cls <- self$class()
      identical(my_cls, cls)
    },



    #' @description
    #' Checks if object is instance of class or subclass of `cls`.
    #' @param cls A class object.
    #' @return TRUE if it inherits from `cls`.
    isKindOf = function(cls) {
      any(
        vapply(
          self$inheritClassesAsList(),
          function(kls) identical(kls, cls),
          logical(1)
        )
      )
    },



    #' @description
    #' Compares object identity with another.
    #' @param other Another object to compare.
    #' @return TRUE if they are the same reference.
    isEqual = function(other) {
      identical(self, other)
    },



    #' @description
    #' Returns a hash of the object based on memory pointer.
    #' @return Integer hash value.
    hash = function() {
      hex_str <- get_pointer_hex(self)
      padded <- sprintf("%016s", tolower(sub("^0x", "", hex_str)))
      unit3 <- substr(padded, 9, 12)
      unit4 <- substr(padded, 13, 16)
      u3 <- strtoi(unit3, base = 16L)
      u4 <- strtoi(unit4, base = 16L)
      return(bitwOr(bitwShiftL(u3, 16L), u4))
    },



    #' @description
    #' Returns the version string of the class associated with this instance.
    classVersion = function() self$class()$robjc_isa$version,



    #' @description
    #' Returns the pointer of instance.
    objectID = function() get_pointer_hex(self),




    #' @description
    #' Returns the list of ancestor classes.
    #' @return list of inheriting classes
    inheritClassesAsList = function() {
      return(self$class()$robjc_isa$inherit_chain)
    },




    ##### RSCopying ####
    #' @description
    #' Returns a shallow copy of the object.
    #' 
    #' For the base \code{RSObject} class, this method simply returns \code{self} unchanged.
    #' This behavior mirrors the default implementation of \code{NSObject}'s \code{copy}
    #' method in Objective-C, which returns the same instance unless overridden by a subclass.
    #' 
    #' Subclasses should override this method if a distinct copy is required,
    #' especially if the object holds internal mutable state.
    #'
    #' @return The object itself (\code{self}).
    #'
    #' @note
    #' The default implementation does not create a new instance. This is safe for
    #' immutable objects. If the object has mutable fields, overriding this method
    #' is strongly recommended to avoid side effects from shared references.
    #'
    #' @examples
    #' \dontrun{
    #' obj <- RSObject$new()
    #' copy <- obj$copy()
    #' identical(obj, copy)  # TRUE
    #' }
    #'
    #' @keywords internal
    copy = function() return(self),



    ##### RSEncoding ####
    #' @description
    #' Returns a list of additional objects to be encoded alongside this instance.
    #'
    #' This method is part of the `RSEncoding` protocol. It is used by the encoder to
    #' determine which instances should be recursively encoded. You should override
    #' this method in your custom subclasses to return all objects that this instance
    #' owns or manages directly.
    #'
    #' Only return instances of `RSObject` subclasses—atomic values or scalar fields should
    #' not be included here. Inherited class metadata and class relationships are handled
    #' automatically and do not need to be manually returned.
    #'
    #' When overriding this method, always call `super$objectsForEncoding()` to collect
    #' objects from parent classes, and combine them with your own declared fields.
    #'
    #' @return A list of objects to be encoded recursively (defaults to an empty list).
    #'
    #' @examples
    #' \dontrun{
    #' MyContainerClass <- R6::R6Class("MyContainerClass", inherit = RSObject,
    #' public = list(
    #'   instance1 = NULL,
    #'   instance2 = NULL,
    #'   initialize = function(obj1, obj2) {
    #'     self$instance1 <- obj1  # RSObject instance
    #'     self$instance2 <- obj2  # RSObject instance
    #'     super$initialize()
    #'   },
    #'   objectsForEncoding = function() {
    #'     inherited <- super$objectsForEncoding()
    #'     custom <- list(self$instance1, self$instance2)
    #'     return(c(inherited, custom))
    #'   }
    #' ))
    #' }
    #'
    #' @seealso
    #' \code{\link[=RSEncoding]{RSEncoding}} – Protocol interface for serialization.
    #' \code{\link[=RSClassEncoder]{RSClassEncoder}} – Main encoder class; see \code{encodeObject()} method for recursive encoding logic.
    #'
    #' @note
    #' Class metadata and superclass information are handled automatically by the encoder.
    #' Do not include class objects or static class-level state in this list.
    #'
    #' @keywords internal
    objectsForEncoding = function() return(list()),


    #' @description
    #' Replace class object for decoding (use on load).
    #' 
    #' @param new_cls Class to replace.
    #' 
    #' @keywords internal
    resolveClassWith = function(new_cls) {
      stopifnot(
        "Class version check." = can_resolve_class(self$class(), new_cls)
      )
      private$.__isa <- new_cls
      invisible(self)
    }
  ),


  private = list(
    # @field .__isa
    # Stores the class object associated with the instance.
    # This is initialized in `initialize()` and used for class checks.
    .__isa = NULL
  )
)