#' Scalar Base Class for the Paired-Class API
#'
#' @description
#' `RSScalar` provides comparison, equality, hashing, and value access for
#' scalar wrappers created by [RSScalarClass()]. It validates a scalar before
#' storing it in private `.val`. Generated NA subclasses receive `na_value`
#' through their initializer.
#'
#' @details
#' A regular subclass may define `initialize(val)` to validate its value type
#' and call `super$initialize(val)`. The factory supplies `na_value` when it
#' constructs the NA subclass's shared instance. Type-specific validation must
#' allow that missing value when `self$class()$isNAClass` is `TRUE`.
#'
#' @return The `RSScalar` class generator.
#'
#' @export
RSScalar <- RSObjectClass(
  "RSScalar",
  version     = "0.0.2 alpha",
  super_class = RSObject,
  public      = list(

    #' @description Validate and store one scalar value.
    #' @param val An atomic scalar or a scalar `POSIXlt` value. A missing value
    #'   is accepted only when the instance belongs to a generated NA subclass.
    #' @return The initialized instance, invisibly.
    initialize = function(val) {
      super$initialize()
      if ((!is.atomic(val) && !inherits(val, "POSIXlt")) || length(val) != 1L) {
        stop("`val` must be a scalar.", call. = FALSE)
      }
      if (isTRUE(is.na(val)) && !isTRUE(self$class()$isNAClass)) {
        na_slot <- self$class()$robjc_isa$slot
        if (robjc_isRobjcObject(na_slot)) {
          na_class <- na_slot$class()$classname
          stop(sprintf("Use %s (via %s$na) for a missing value.",
                       na_class, self$class()$classname), call. = FALSE)
        }
        stop("This scalar class has no NA companion.", call. = FALSE)
      }
      private$.val <- val
      invisible(self)
    },



    #' @description Compare this scalar with another scalar in the same class
    #'   family. Missing values are ordered according to `na.last`.
    #' @param other Another `RSScalar` instance in the same class family.
    #' @param na.last Whether a missing value sorts after nonmissing values.
    #' @return `-1L`, `0L`, or `1L`.
    compare = function(other, na.last = TRUE) {
      if (!is.logical(na.last) || length(na.last) != 1L || is.na(na.last)) {
        stop("`na.last` must be TRUE or FALSE.", call. = FALSE)
      }
      if (!robjc_isRobjcObject(other) || !other$isKindOf(RSScalar) ||
          !(self$isKindOf(other$class()) || other$isKindOf(self$class()))) {
        stop("Scalar classes are not comparable.", call. = FALSE)
      }
      self_na <- isTRUE(is.na(self$value()))
      other_na <- isTRUE(is.na(other$value()))
      if (self_na && other_na) return(0L)
      if (self_na) return(if (na.last) 1L else -1L)
      if (other_na) return(if (na.last) -1L else 1L)
      values <- xtfrm(c(self$value(), other$value()))
      if (values[[1L]] < values[[2L]]) return(-1L)
      if (values[[1L]] > values[[2L]]) return(1L)
      0L
    },



    #' @description Give a concise description of the wrapped value.
    #' @return A character scalar.
    description = function() {
      sprintf("<%s %s> %s", self$class()$classname,
              get_pointer_hex(self), as.character(private$.val))
    },



    #' @description Check equality using `compare()`.
    #' @param other Another scalar in the same class family.
    #' @return `TRUE` when the values compare equal.
    isEqual = function(other) self$compare(other) == 0L,



    #' @description Hash the wrapped scalar.
    #' @return An integer hash code.
    hash = function() RSHashOperator$instance$hashForScalar(private$.val),



    #' @description Return the stored R scalar.
    #' @return The wrapped value.
    value = function() private$.val
  ),
  private = list(
    .val = NULL,
    .comparable = function(other) {
      robjc_isRobjcObject(other) && other$isKindOf(RSScalar) &&
        (self$isKindOf(other$class()) || other$isKindOf(self$class()))
    }
  )
)



# The base class itself is neither a regular wrapper nor an NA companion.
RSScalar$isNAClass <- NULL
lockBinding("isNAClass", RSScalar)
