#' @include RSComparable.r
#' @title Scalar Base Class for the Paired-Class API
#'
#' @description
#' `RSScalar` is an abstract base class that provides comparison, equality,
#' hashing, and value access for
#' scalar wrappers created by [RSScalarClass()]. It validates a scalar before
#' storing it in private `.val`. Generated NA subclasses receive `na_value`
#' through their initializer.
#'
#' @details
#' A regular subclass may define `initialize(val)` to validate its value type
#' and call `super$initialize(val)`. The factory supplies `na_value` when it
#' constructs the NA subclass's shared instance. Type-specific validation must
#' allow that missing value when `self$class()$isNAClass` is `TRUE`.
#' Singleton NA companions reject direct construction; use the corresponding
#' class's `$na` instance. A non-singleton NA companion accepts only an `NA`
#' value when constructed directly. `NaN` remains a regular scalar value.
#'
#' @return The `RSScalar` class generator.
#'
#' @export
RSScalar <- RSObjectClass(
  "RSScalar",
  version     = "0.0.4 alpha",
  super_class = RSObject,
  protocols   = list(RSComparable),
  public      = list(

    #' @description Validate and store one scalar value. NA companion classes
    #'   accept only `NA`, not `NaN` or a nonmissing value.
    #' @param val An atomic scalar or a scalar `POSIXlt` value. `NA` is accepted
    #'   only in a generated NA subclass; `NaN` remains a regular scalar value.
    #'   Values with a `dim` attribute are rejected.
    #' @return The initialized instance, invisibly.
    initialize = function(val) {
      if (self$isMemberOf(RSScalar))
        stop(
          "RSScalar is abstract and cannot be instantiated directly.",
          call. = FALSE
        )
      super$initialize()
      if ((!is.atomic(val) && !inherits(val, "POSIXlt")) ||
          length(val) != 1L || !is.null(dim(val)))
        stop("`val` must be a scalar.", call. = FALSE)
      if (isTRUE(self$class()$isNAClass) &&
          !(isTRUE(is.na(val)) && !isTRUE(is.nan(val)))) {
        stop("NA companion classes require an NA value.", call. = FALSE)
      }
      #! DO NOT REMOVE
      #  RSScalarClass creates the NA companion with isNAClass = TRUE, so its
      #  initializer must accept NA. This branch is still needed when a caller
      #  passes NA to the regular class's $new();
      if (isTRUE(is.na(val)) &&
          !isTRUE(is.nan(val)) &&
          !isTRUE(self$class()$isNAClass) &&
          !identical(self$class(), RSFactor)) {
        na_slot <- self$class()$robjc_isa$slot
        if (robjc_isRobjcObject(na_slot)) {
          na_class <- na_slot$class()$classname
          stop(
            sprintf(
              "Use %s (via %s$na) for a missing value.",
              na_class, self$class()$classname
            ),
            call. = FALSE
          )
        }
        stop("This scalar class has no NA companion.", call. = FALSE)
      }
      private$.val <- val
      invisible(self)
    },



    #' @description Compare this scalar with another scalar in the same class
    #'   family using base R's sorting order. Missing values (including `NaN`)
    #'   are tied and placed according to `na.last`.
    #' @param other Another `RSScalar` instance in the same class family.
    #' @param na.last Whether a missing value sorts after nonmissing values.
    #'   Coerced with `as.logical()`; the result must be one nonmissing value.
    #' @return `-1L`, `0L`, or `1L`.
    compare = function(other, na.last = TRUE) {
      na.last <- tryCatch(as.logical(na.last), error = function(e) NA)
      if (!is.logical(na.last) || length(na.last) != 1L || is.na(na.last))
        stop("`na.last` must be TRUE or FALSE.", call. = FALSE)
      if (!private$.comparable(other))
        stop("Scalar classes are not comparable.", call. = FALSE)
      self_na <- isTRUE(is.na(self$value()))
      other_na <- isTRUE(is.na(other$value()))
      if (self_na && other_na) return(0L)
      if (self_na) return(if (na.last) 1L else -1L)
      if (other_na) return(if (na.last) -1L else 1L)
      values <- xtfrm(c(self$nativeValue(), other$nativeValue()))
      if (values[[1L]] < values[[2L]]) return(-1L)
      if (values[[1L]] > values[[2L]]) return(1L)
      0L
    },



    #' @description Give a concise description of the wrapped value.
    #' @return A character scalar.
    description = function() {
      sprintf(
        "<%s %s> %s",
        self$class()$classname,
        get_pointer_hex(self), as.character(private$.val)
      )
    },



    #' @description Check key equality for compatible scalars. `NA` equals `NA`
    #'   and `NaN` equals an identical `NaN` value so they can be used as keys.
    #' @param other An object to compare.
    #' @return `TRUE` when the values compare equal; `FALSE` for incompatible objects.
    isEqual = function(other) {
      if (!private$.comparable(other))
        return(FALSE)
      self_nan  <- isTRUE(is.nan(self$value()))
      other_nan <- isTRUE(is.nan(other$value()))
      if (self_nan || other_nan)
        return(self_nan && other_nan && identical(self$value(), other$value()))
      self_na   <- isTRUE(is.na(self$value()))
      other_na  <- isTRUE(is.na(other$value()))
      if (self_na || other_na)
        return(isTRUE(self_na && other_na))
      isTRUE(self$value() == other$value())
    },



    #' @description Return the original native R value. This can differ from
    #'   `value()`, which returns the scalar's object-level value.
    #' @return The native R scalar.
    nativeValue = function() self$value(),



    #' @description Compare native R values with base R's equality and ordering
    #'   rules. Unlike `compare()`, this permits different scalar classes when
    #'   base R can compare their values. Missing values (including `NaN`) are
    #'   tied and placed according to `na.last`.
    #' @param other Another `RSScalar` instance.
    #' @param na.last Whether a missing value sorts after nonmissing values.
    #'   Coerced with `as.logical()`; the result must be one nonmissing value.
    #' @return `-1L`, `0L`, or `1L`. An error is raised when base R has no
    #'   ordering for unequal values.
    nativeCompare = function(other, na.last = TRUE) {
      na.last <- tryCatch(as.logical(na.last), error = function(e) NA)
      if (!is.logical(na.last) || length(na.last) != 1L || is.na(na.last))
        stop("`na.last` must be TRUE or FALSE.", call. = FALSE)
      if (!robjc_isRobjcObject(other) || !other$isKindOf(RSScalar))
        stop("`other` must be an RSScalar object.", call. = FALSE)
      left <- self$nativeValue()
      right <- other$nativeValue()
      if (is.null(left) || is.null(right))
        stop("Null objects are not comparable.", call. = FALSE)
      left_na <- isTRUE(is.na(left))
      right_na <- isTRUE(is.na(right))
      if (left_na && right_na) return(0L)
      if (left_na) return(if (na.last) 1L else -1L)
      if (right_na) return(if (na.last) -1L else 1L)
      if (isTRUE(left == right)) return(0L)
      less <- left < right
      if (isTRUE(less)) return(-1L)
      if (length(less) != 1L || is.na(less))
        stop("Native values have no ordering.", call. = FALSE)
      greater <- left > right
      if (isTRUE(greater)) return(1L)
      stop("Native values have no ordering.", call. = FALSE)
    },



    #' @description Hash the wrapped scalar. `NA` and `NaN` hash to `0L`.
    #' The result passes through [normalize_hash()].
    #' @return One non-missing integer hash code.
    hash = function() {
      hash_code <- if (isTRUE(is.na(private$.val))) 0L else
        RSHashOperator$instance$hashForScalar(private$.val)
      normalize_hash(hash_code)
    },



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
