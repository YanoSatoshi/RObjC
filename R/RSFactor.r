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
  version = "0.0.2 alpha",
  super_class = RSString,
  is_na_class = FALSE,
  public = list(



    #' @description
    #' Initializes an `RSFactor` object by wrapping a scalar R factor value.
    #' The actual value is stored as a character string (via `as.character()`),
    #' and the factor’s levels and ordered flag are separately preserved.
    #'
    #' @param val A scalar factor value.
    #' @return An initialized `RSFactor` instance (invisible).
    initialize = function(val) {
      if (!is.factor(val)) stop("RSFactor can only wrap factor values.")
      super$initialize(as.character(val))
      if (is.na(val) && self$isMemberOf(RSFactor)) {
        stop("Use RSNAFactor$new() for NA factor.")
      }
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
    #' Compares two `RSFactor` objects based on the numeric representation of their levels.
    #' Preserves ordered semantics and supports comparison even in the presence of `NA`.
    #'
    #' @param other Another `RSFactor` instance to compare with.
    #' @param na.last Logical. If `TRUE`, `NA` values are treated as greater than any non-NA.
    #' @return -1, 0, or 1, indicating whether this object is less than, equal to, or greater than `other`.
    compare = function(other, na.last = TRUE) {
      if (!private$.comparable(other)) {
        stop(sprintf(
          "Abort trying to compare %s and %s",
          self$class()$classname, other$class()$classname
        ))
      }
      na_val <- if (na.last) .Machine$double.xmax else .Machine$double.xmin
      val1 <- as.numeric(self$asFactor())
      val2 <- as.numeric(other$asFactor())
      val1 <- if (is.na(val1)) na_val else val1
      val2 <- if (is.na(val2)) na_val else val2
      return(sign(val1 - val2))
    }
  ),
  private = list(
    .levels = NULL,
    .ordered = NULL
  )
)


#' @title RSNAFactor: Wrapper for NA Factor Scalar
#'
#' @description
#' `RSNAFactor` is a subclass of `RSFactor` used to represent `NA` factor values in a
#' structured object-oriented manner. This allows `NA` to behave consistently with other
#' `RSScalar`-derived objects in comparison, hashing, and set operations.
#'
#' The instance has no levels and is not ordered.
#'
#' @note
#' Normally, this class is constructed internally via `RSFactor_withFactor()`.
#'
#' @examples
#' na_f <- RSFactor_withFactor(factor(NA_character_))
#' na_f$ordered()  # FALSE
#' na_f$levels()   # NULL
#'
#' @export
RSNAFactor <- RSScalarClass(
  "RSNAFactor",
  version = "0.0.2 alpha",
  super_class = RSFactor,
  is_na_class = TRUE,
  public = list(

    #' @description
    #' Initializes an `RSNAFactor` object representing a missing (`NA`) factor value.
    #' Internally sets the value to `NA`, with `NULL` levels and `ordered = FALSE`.
    #'
    #' @return An initialized `RSNAFactor` instance (invisible).
    initialize = function() {
      super$initialize(factor(NA_character_))
      private$.levels <- NULL
      private$.ordered <- FALSE
      invisible(self)
    }
  )
)
