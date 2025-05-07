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
#'
#' @section Internal Fields:
#' - `.__class__` – Cached class object.
#' - `.__protocols__` – Vector of protocol names (if any) the object claims conformance to.
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
  ### class name ###
  "RSObject",
  cloneable = FALSE,
  public = list(
    #' @description
    #' Initializes the RSObject and assigns its class metadata internally.
    #' This should not be called directly by users.
    initialize = function() {
      # get Class Object
      class_name <- class(self)[1]  ## Only here, get classname from func class().  # nolint
      base_env   <- environment(self$initialize)
      search_env <- NULL
      if (identical(base_env, .GlobalEnv)) {
        search_env <- parent.env(base_env)
      } else {
        search_env <- base_env
      }
      class_def  <- get(class_name, envir = search_env, inherits = TRUE)
      private$.__class__ <- class_def
      invisible(self)
    },
    #' @description
    #' Returns the internal class object representing the instance’s class.
    #' Equivalent to Objective-C's `[self class]`.
    class = function() {
      private$.__class__
    },
    #' @description
    #' Checks whether the object conforms to the given protocol name.
    #' @param protocol A character string representing the protocol name.
    #' @return TRUE if the object conforms to the protocol; FALSE otherwise.
    conformsTo = function(protocol) {
      if (is.null(private$.__protocols__)) return(FALSE)
      protocol %in% private$.__protocols__
    },
    #' @description
    #' Checks whether the object implements a method with the given name.
    #' @param method A character string representing the method name.
    #' @return TRUE if the method exists and is callable; FALSE otherwise.
    respondsTo = function(method) {
      in_public <-
        exists(method, where = self, inherits = TRUE) &&
        is.function(self[[method]])
      in_private <-
        exists(method, where = private, inherits = TRUE) &&
        is.function(self[[method]])
      in_public || in_private
    },
    #' @description
    #' Returns a concise textual description of the object.
    #' Includes class name and memory pointer.
    #' @return A character string like "<RSObject 0x1234abcd>".
    description = function() {
      paste0("<", self$class()$classname, " ", private$.pointerHex(), ">")
    },
    #' @description
    #' Returns a verbose description of the object including all public fields.
    #' Useful for debugging and logging.
    #' @return A character string with structure summary.
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
    #' Prints a short description of the object to the console.
    #' Calls `description()` internally.
    print = function() {
      cat(self$description(), "\n")
      invisible(self)
    },
    #' @description
    #' Checks whether the object is a direct instance of the specified class.
    #' @param cls A class object to compare against.
    #' @return TRUE if the object is a direct instance of `cls`; FALSE otherwise.
    isMemberOf = function(cls) {
      my_cls <- self$class()
      identical(my_cls, cls)
    },
    #' @description
    #' Checks whether the object is an instance of the specified class or its subclass.
    #' @param cls A class object to compare against.
    #' @return TRUE if the object inherits from `cls`; FALSE otherwise.
    isKindOf = function(cls) {
      any(
        vapply(
          private$.inheritClasses(),
          function(kls) identical(kls, cls),
          logical(1)
        )
      )
    },
    #' @description
    #' Checks whether the object is the same reference as another object.
    #' @param other An object to compare with.
    #' @return TRUE if the two objects are identical.
    isEqual = function(other) {
      identical(self, other)
    },
    #' @description
    #' Computes a numeric hash based on the memory address of the object.
    #' The result is suitable for fast hashing in sets or dictionaries.
    #' @return An integer hash value.
    hash = function() {
      hex_str <- private$.pointerHex()
      padded <- sprintf("%016s", tolower(sub("^0x", "", hex_str)))
      unit3 <- substr(padded, 9, 12)
      unit4 <- substr(padded, 13, 16)
      u3 <- strtoi(unit3, base = 16L)
      u4 <- strtoi(unit4, base = 16L)
      return(bitwOr(bitwShiftL(u3, 16L), u4))
    }
  ),
  private = list(
    #' @field .__class__
    #' Stores the class object associated with the instance.
    #' This is initialized in `initialize()` and used for class checks.
    .__class__ = NULL,
    .__protocols__ = NULL,
    #' @description
    #' Extracts the hexadecimal representation of the object’s memory environment.
    #' Used internally by `description()` and `hash()`.
    #' @return A character string such as "0x1234abcd".
    .pointerHex = function() {
      addr <- capture.output(environment(self$print))
      # addr <- capture.output(self$.__enclos_env__)
      hex_match <- sub("^<environment: (0x[0-9a-f]+)>$", "\\1", addr)
      if (is.na(hex_match)) {
        stop("RSObject internal pointer (.ptr) could not be extracted.")
      }
      return(hex_match)
    },
    #' @description
    #' Computes and caches the full class inheritance chain of the object.
    #' Used by `isKindOf()`.
    #' @return A list of class objects representing the inheritance chain.
    .inheritClasses = function() {
      cls <- private$.__class__
      if (!is.null(cls$rs_inherit_chain)) return(cls$rs_inherit_chain)
      rs_inherit_chain <- list()
      while (!is.null(cls)) {
        size <- length(rs_inherit_chain)
        rs_inherit_chain[[size + 1]] <- cls
        if (!is.symbol(cls$inherit)) {
          cls <- cls$inherit
        } else {
          cls <- eval(cls$inherit)
        }
      }
      cls <- private$.__class__
      cls$rs_inherit_chain <- rs_inherit_chain
      lockBinding("rs_inherit_chain", cls)
      return(cls$rs_inherit_chain)
    }
  )
);
RSObject$.__class__ <- RSObject
RSObject$rs_inherit_chain <- NULL
RSObject$new()
