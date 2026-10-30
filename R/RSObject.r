#' @include robjc-runtime.r
#'
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
#' - `isMemberOf(cls)` – Checks if the object is a direct instance of the given class.
#' - `isKindOf(cls)` – Checks if the object inherits from the given class (directly or indirectly).
#' - `isEqual(other)` – Returns `TRUE` if the reference is identical to `other`.
#' - `hash()` – Computes an integer hash from the object's environment address.
#' - `objectID()` – Returns a pointer-like identity based on the instance's
#'   environment address.
#'
#' @section Internal Fields:
#' - `.__isa` – Cached class object.
#'
#' @section Notes:
#' - The hash method uses part of the object's environment address to produce
#'   an integer hash. Different objects can have the same hash.
#' - `hash()` applies [robjc_normalize_hash()] to its final integer, mapping R's
#'   `NA_integer_` bit pattern to `0L`. Custom `hash()` methods should do the same.
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
  class     = FALSE,
  cloneable = FALSE,
  portable  = TRUE,
  active    = NULL,
  inherit   = NULL,
  public    = list(
    #' @description Initializes a new instance.
    #' @return The initialized object itself (invisibly).
    initialize = function() {
      class(self) <- vapply(
        robjc_inheritChain(private$.__isa),
        function(klass) klass$classname,
        character(1)
      )
      invisible(self)
    },


    #' @description Returns the class-object.
    #' @return The class-object (generator).
    class = function() private$.__isa$class,


    #' @description Returns the super-class.
    #' @return The object's super-class (generator).
    superClass = function() self$class()$robjc_isa$super,


    #' @description Checks whether the object's class declared adoption of a
    #'   protocol. This does not verify method implementations.
    #' @param protocol A protocol class generator.
    #' @return TRUE if conforms, FALSE otherwise.
    conformsTo = function(protocol) {
      if (!robjc_isProtocol(protocol))
        stop("`protocol` must be a kind of RSProtocol.")
      for (klass in robjc_inheritChain(private$.__isa)) {
        for (aProtocol in klass$robjc_isa$protocols) {
          if (protocol_conformsTo(aProtocol, protocol))
            return(TRUE)
        }
      }
      return(FALSE)
    },



    #' @description
    #' Checks if the object exposes a public method.
    #' @param method One non-empty, non-missing character string naming the method.
    #' @return TRUE if the method is publicly callable.
    respondsTo = function(method) {
      if (!is.character(method) || length(method) != 1L ||
          is.na(method) || !nzchar(method)) {
        stop("`method` must be one non-empty character string.", call. = FALSE)
      }
      exists(method, where = self, inherits = TRUE) && is.function(self[[method]])
    },



    #' @description
    #' Returns a compact description like "<Class 0xabc123>".
    #' @return A character string describing the object.
    description = function() {
      paste0("<", self$class()$classname, " ", robjc_get_pointer(self), ">")
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
          val_str <- utils::capture.output(str(value, max.level = 1))
          paste(val_str, collapse = " ")
        },
        character(1)
      )
      fields_info <- paste(fields, ": ", field_values, collapse =", ")
      paste0(descript, "{", fields_info, "}")
    },



    #' @description
    #' Checks if object is an exact instance of class `cls`.
    #' @param cls A class object.
    #' @return TRUE if the class matches exactly.
    isMemberOf = function(cls) identical(self$class(), cls),



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
    isEqual = function(other) isTRUE(identical(self, other)),



    #' @description
    #' Returns an integer hash derived from this instance's environment address.
    #' The result passes through [robjc_normalize_hash()],
    #' so the `NA_integer_` bit pattern maps to `0L`.
    #' @return One non-missing integer hash value.
    hash = function() {
      hex_str <- robjc_get_pointer(self)
      padded <- sprintf("%016s", tolower(sub("^0x", "", hex_str)))
      unit3 <- substr(padded, 9, 12)
      unit4 <- substr(padded, 13, 16)
      u3 <- strtoi(unit3, base = 16L)
      u4 <- strtoi(unit4, base = 16L)
      hash_code <- bitwOr(bitwShiftL(u3, 16L), u4)
      robjc_normalize_hash(hash_code)
    },



    #' @description
    #' Returns the hexadecimal address of this instance's environment. Within
    #' the current R session, the address serves as a pointer-like identity.
    #' @return A hexadecimal address string.
    objectID = function() robjc_get_pointer(self),




    #' @description
    #' Returns the list of ancestor classes.
    #' @return list of inheriting classes
    inheritClassesAsList = function() robjc_inheritChain(private$.__isa),




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



    #' @description Returns an empty list for legacy serialization hooks.
    #'   The keyed archiver uses `encodeWithCoder()` instead. This method is
    #'   not part of [RSCoding].
    #' @return An empty list.
    #'
    #' @keywords internal
    objectsForEncoding = function() list(),


    #' @description
    #' Replace class object for decoding (use on load), then update the
    #' instance's S3 class names to match the resolved class hierarchy.
    #' 
    #' @param new_cls Class to replace.
    #' 
    #' @keywords internal
    resolveClassWith = function(new_cls) {
      stopifnot(
        "Class version check." = can_resolve_class(self$class(), new_cls)
      )
      private$.__isa <- new_cls$robjc_isa
      class(self) <- vapply(
        robjc_inheritChain(private$.__isa),
        function(klass) klass$classname,
        character(1)
      )
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
