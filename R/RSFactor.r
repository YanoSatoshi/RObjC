#' @include RSString.r
#' @title RSFactor: Wrapper for R's Factor Scalar Values
#'
#' @description
#' `RSFactor` is a subclass of `RSString` designed to wrap R factor values while preserving
#' their level definitions and ordering information. This enables safe scalar-level comparison
#' and hashing while retaining the semantics of categorical variables.
#'
#' During initialization, the factor is converted to a character string for value storage,
#' while `levels()` and `ordered()` provide access to the original factor metadata.
#'
#' @section Methods:
#' - `asFactor()` – Reconstructs the original factor with levels and ordering
#' - `levels()` – Returns the vector of factor levels
#' - `ordered()` – Returns TRUE if the factor was ordered
#' - `compare(other, na.last = TRUE)` – Compares factor values respecting ordering
#'
#' @section Usage:
#' Use \code{RSFactor_withFactor(val)} to construct safely from a factor value.
#'
#' @examples
#' f <- factor("low", levels = c("low", "medium", "high"), ordered = TRUE)
#' obj <- RSFactor_withFactor(f)
#' obj$levels()      # "low" "medium" "high"
#' obj$ordered()     # TRUE
#' obj$asFactor()    # reconstructs original factor
#'
#' @seealso RSNAFactor
#'
#' @export
RSFactor <- RSScalarClass(
  "RSFactor",
  version     = "0.0.4 alpha",
  super_class = RSString,
  na_value    = factor(NA_character_),
  na_singleton = FALSE,
  public      = list(

    #' @description
    #' Initializes an `RSFactor` object by wrapping a scalar R factor value.
    #' The actual value is stored as a character string (via `as.character()`),
    #' and the factor’s levels and ordered flag are separately preserved.
    #'
    #' @param val A scalar with the standard `factor` or `ordered`/`factor`
    #'   class, without an S3 subclass.
    #' @return An initialized `RSFactor` instance (invisible).
    initialize = function(val) {
      standard_class <- identical(class(val), "factor") ||
        identical(class(val), c("ordered", "factor"))
      if (!standard_class || length(val) != 1L || !is.null(dim(val)))
        stop("`val` must be a factor scalar.", call. = FALSE)
      if (is.na(val) && !isTRUE(self$class()$isNAClass))
        stop("Use `RSNAFactor` for NA", call. = FALSE)
      super$initialize(as.character(val))
      private$.levels  <- levels(val)
      private$.ordered <- is.ordered(val)
      invisible(self)
    },



    #' @description
    #' Reconstructs and returns the original R factor from the internal character value,
    #' using the preserved levels and ordering flag.
    #'
    #' @return A scalar `factor` object with its original levels and ordering.
    asFactor = function() {
      factor(
        self$value(),
        levels  = private$.levels,
        ordered = private$.ordered
      )
    },



    #' @description Return the original factor as a native R value.
    #' @return A scalar factor preserving levels and ordering.
    nativeValue = function() self$asFactor(),



    #' @description
    #' Returns the vector of levels associated with the original factor.
    #'
    #' @return A character vector of levels, or `NULL` if unavailable.
    levels = function() {
      private$.levels
    },



    #' @description
    #' Indicates whether the original factor was ordered.
    #'
    #' @return `TRUE` if the factor was ordered; otherwise `FALSE`.
    ordered = function() {
      if (is.null(private$.ordered)) return(FALSE)
      return(private$.ordered)
    },



    #' @description
    #' Compare factors with matching levels using base R's sorting order.
    #' This applies to both ordered and unordered factors. Strings have no
    #' shared three-way sort order here. Levels must match even when either
    #' value is missing; missing values then follow `na.last`.
    #'
    #' @param other A comparable `RSFactor` instance.
    #' @param na.last If `TRUE`, `NA` values are treated as greater than any
    #'   non-NA. Coerced with `as.logical()`; the result must be one nonmissing
    #'   value.
    #' @return `-1L`, `0L`, or `1L`, indicating whether this object is less than,
    #'   equal to, or greater than `other`.
    compare = function(other, na.last = TRUE) {
      na.last <- tryCatch(as.logical(na.last), error = function(e) NA)
      if (!is.logical(na.last) || length(na.last) != 1L || is.na(na.last))
        stop("`na.last` must be TRUE or FALSE.", call. = FALSE)
      if (!private$.comparable(other))
        stop("Scalar classes are not comparable.", call. = FALSE)
      if (!other$isKindOf(RSFactor))
        stop("Factor and string values have no shared ordering.", call. = FALSE)
      if (!identical(self$levels(), other$levels()))
        stop("Factor level sets differ.", call. = FALSE)
      if (isTRUE(is.na(self$value())) || isTRUE(is.na(other$value())))
        return(super$compare(other, na.last = na.last))
      super$compare(other, na.last = na.last)
    }
  ),
  private = list(
    .levels = NULL,
    .ordered = NULL
  )
)



#' @title RSNAFactor
#' @description Factor NA companion for RSFactor. Generated by [RSScalarClass()] when
#'   `RSFactor` is declared. `RSFactor_withFactor()` creates an instance that
#'   retains the input's levels and ordering. `RSFactor$na` is the factory's
#'   default instance, with no input-specific metadata.
#' @return The NA class generator.
#' @export
RSNAFactor <- RSFactor$na$class()
