#' @title RSScalar: Abstract Scalar Wrapper Base Class
#'
#' @description
#' `RSScalar` is the abstract base clas for all scalar wrapper classes in the RS framework,
#' designed to emulate Objective-C's value object philosophy.
#' It provides standard scalar behavior such as comparison, equality, hashing, and internal value access,
#' and acts as a foundation for types like `RSNumber`, `RSString`, `RSDate`, etc.
#'
#' Subclasses are expected to define a class-level field `isNAClass` (TRUE/FALSE) to distinguish
#' between regular values and NA-wrapper variants (e.g., `RSNumber` vs `RSNANumber`).
#' The constructor enforces length-1 scalar input and prohibits NA unless allowed by the subclass.
#'
#' @section Public Methods:
#' - `initialize(val)` – Validates and stores a scalar value.
#' - `compare(other, na.last = TRUE)` – Compares this scalar with another.
#' - `isEqual(other)` – Returns TRUE if both values are equal.
#' - `hash()` – Returns a numeric hash for the scalar value.
#' - `value()` – Returns the raw wrapped value.
#' - `copyWithEnv(env)` – Copies the object into another environment.
#' - `description()` – Human-readable description string.
#'
#' @section Usage:
#' Use \code{RSScalar$valueWith(val)} as a convenience constructor.
#' It automatically routes to the appropriate RS*** subclass based on the type of \code{val}.
#'
#' @note
#' Subclasses must call `super$initialize(val)` after setting their own fields.
#' A companion `RSNA*` class should be provided for representing NA values safely.
#'
#' @examples
#' num <- RSNumber$numberWith(42)
#' str <- RSString$stringWith("hello")
#' num$compare(RSNumber$numberWith(100))  # returns -1
#'
#' @export
RSScalar <- R6::R6Class(
  ### class name ###
  "RSScalar",
  cloneable = FALSE,
  inherit = RSObject,
  public = list(

    #' @description
    #' Initializes an `RSScalar` instance by validating and storing a single scalar value.
    #' This constructor enforces that the input value has a length of exactly 1, and
    #' verifies whether NA values are allowed based on the class's `isNAClass` flag.
    #'
    #' For inheriting classes (e.g., `RSNumber`, `RSString`), the `isNAClass` field must be
    #' defined explicitly to distinguish between regular scalar classes (`isNAClass = FALSE`)
    #' and their NA counterparts (`isNAClass = TRUE`). If a class does not allow NA values
    #' but receives one, the constructor will raise an informative error suggesting the
    #' appropriate NA class.
    #'
    #' Inheriting classes should override this method to add type-specific input validation,
    #' while still calling `super$initialize(val)` for scalar enforcement.
    #' 
    #' @param val Wrapping value
    initialize = function(val) {
      super$initialize()
      ## CHECK LENGTH ARG.
      ## IF NOT 1, THEN
      if (length(val) != 1L) {
        stop(
          sprintf(
            "%s can only accept length 1L vector.",
            self$class()$classname
          )
        )
      }
      stopifnot(!is.null(self$class()$isNAClass))
      # IN INHERITING, CLASS DO AS FOLLOWS
      #   SomeClass <- R6::R6Class(...)  # nolint
      #   SomeClass$isNAClass <- TRUE/FALSE  ... TRUE on RS*** class, FALSE on RSNA*** class  # nolint
      #   # additonary option
      #   SomeClass$***With <- function(val) {
      #     if (is.na(val)) return(RSNA***$new())
      #     return(RS***$new(val))
      #   }
      if (is.na(val) && !self$class()$isNAClass) {
        stop(
          sprintf("use %s for NA.", sub("RS", "RSNA", self$class()$classname))
        )
      }
      ## SET VAL
      private$.val <- val
      private$.hash_operator <- RSHashOperator$module()
      invisible(self)
      ## IN INHERITING CLASS, DO AS FOLLOWS
      # initialize = functin(val) {
      #   if (!inherits(val, 'some R class name')) stop("val must be of class some_class_name") # nolint
      #   super$initialize(val)  # nolint
      #   invisible(self)  # nolint
      # }
      ## END
    },

    #' @description
    #' Compares the scalar value with another RSScalar-derived object.
    #' Returns -1, 0, or 1 depending on comparison order.
    #' @param other Another RSScalar object.
    #' @param na.last Logical; whether NA should be treated as highest or lowest.
    #' @return Integer: -1 if less than, 0 if equal, 1 if greater than.
    compare = function(other, na.last = TRUE) {
      if (!private$.comparable(other)) {
        stop(sprintf(
          "Abort trying to compare %s and %s",
          self$class()$classname, other$class()$classname
        ))
      }
      na_val <- if (na.last) .Machine$double.xmax else .Machine$double.xmin
      # if do xtfrm(NULL), it returns NA
      vec  <- xtfrm(c(self$value(), other$value()))
      val1 <- if (is.na(vec[1])) na_val else vec[1]
      val2 <- if (is.na(vec[2])) na_val else vec[2]
      return(sign(val1 - val2))
    },

    #' @description
    #' Returns a concise textual representation of the scalar object.
    #' The output includes the class name, a hexadecimal pointer string identifying the instance,
    #' and the scalar value it wraps. This is primarily used for debugging or printing
    #' informative summaries of scalar objects.
    description = function() {
      sprintf(
        "<%s %s> %s",
        self$class()$classname, private$.pointerHex(),
        as.character(private$.val)
      )
    },

    #' @description
    #' Checks whether two scalar objects are equal.
    #' @param other Another RSScalar object.
    #' @return TRUE if values are considered equal; otherwise FALSE.
    isEqual = function(other) {
      self$compare(other) == 0L
    },

    #' @description
    #' Returns a numeric hash of the wrapped scalar value.
    #' @return An integer suitable for use in hashing-based structures (e.g., sets, dictionaries).
    hash = function() {
      private$.hash_operator$hashForScalar(private$.val)
    },

    #' @description
    #' Returns the underlying R scalar value stored in the object.
    #' @return A scalar value (numeric, character, logical, etc.).
    value = function() {
      private$.val
    },

    #' @description
    #' Creates a copy of the scalar object within the specified environment.
    #' @param env An environment where the new instance should be evaluated.
    #' @return A cloned RSScalar object with identical value, bound to `env`.
    copyWithEnv = function(env) {
      if (identical(env, environment(self))) return(self)
      klass <- self$class()
      if (!klass$isNAClass) {
        do.call(klass$new, list(private$.val), envir = env)
      } else {
        do.call(klass$new, envir = env)
      }
    }

  ),

  private = list(
    # @field .val
    # (Private) Stores the underlying scalar value.
    # Assigned during initialization; accessed via `value()`.
    .val = NULL,

    # @field .hash_operator
    # (Private) Holds an instance of `RSHashOperator`, used to compute hash values.
    # Initialized during `initialize()`.
    .hash_operator = NULL,

    # @description
    # (Private) Determines if this object and another scalar are comparable,
    # considering both class and NA-type alignment.
    # - If one is an NA wrapper and the other is not, uses inheritance checks.
    # - If both are of the same type (either NA or non-NA), uses strict `isMemberOf`.
    # @param other An RSScalar-derived object.
    # @return TRUE if comparable; otherwise FALSE.
    .comparable = function(other) {
      if (xor(self$class()$isNAClass, other$class()$isNAClass)) {
        # self or other is kind of NAClass
        if (length(class(self)) < length(class(other))) {
          return(other$isKindOf(self$class()))
        } else {
          return(self$isKindOf(other$class()))
        }
      } else {
        # both self and other are NAClasses or Not-NAClasses
        return(self$isMemberOf(other$class()))
      }
    }
  )
)
RSScalar$isNAClass = NULL  # nolint




# @title Wrap a Native R Scalar into RSScalar Object
#
# @name valueWith
# 
# @description
# Converts a native scalar R value (numeric, character, logical, factor, Date, POSIXct)
# into an appropriate RSScalar subclass. This function acts as a factory method and
# delegates to `RSNumber`, `RSString`, `RSBoolean`, etc., based on the value's class.
#
# This function enforces that only length-1 values can be wrapped. If the value is NULL,
# an `RSNull` singleton is returned.
#
# @param val A scalar value of supported R base types.
# @return An object of class `RSScalar` (or subclass), such as `RSNumber`, `RSString`, etc.
#
# @examples
# RSScalar$valueWith(123)        # returns RSNumber
# RSScalar$valueWith("hello")    # returns RSString
# RSScalar$valueWith(Sys.Date()) # returns RSDate
# @export
RSScalar$valueWith <- function(val) {
  if (is.null(val)) {
    return(RSNull$null())
  }
  if (length(val) != 1) {
    stop(sprintf(
      "Only scalar (length 1) values can be wrapped. Provided length: %d",
      length(val)
    ))
  }
  cls <- class(val)[1]
  if (cls == "numeric" || cls == "integer") {
    return(RSNumber$numberWith(val))
  } else if (cls == "character") {
    return(RSString$stringWith(val))
  } else if (cls == "factor") {
    return(RSFactor$factorWith(val))
  } else if (cls == "logical") {
    return(RSBoolean$boolWith(val))
  } else if (cls == "POSIXct") {
    return(RSDateTime$dateTimeWith(val))
  } else if (cls == "Date") {
    return(RSDate$dateWith(val))
  } else {
    stop(sprintf("Unsupported scalar type: %s", cls))
  }
}
