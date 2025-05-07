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
#' @examples
#' f <- factor("low", levels = c("low", "medium", "high"), ordered = TRUE)
#' obj <- RSFactor$factorWith(f)
#' obj$levels()      # "low" "medium" "high"
#' obj$ordered()     # TRUE
#' obj$asFactor()    # reconstructs original factor
#'
#' @seealso RSNAFactor
#'
#' @export
RSFactor <- R6::R6Class(
  "RSFactor",
  cloneable = FALSE,
  inherit = RSString,
  public = list(
    initialize = function(val) {
      if (!is.factor(val)) stop("RSFactor can only wrap factor values.")
      super$initialize(as.character(val))
      private$.levels  <- levels(val)
      private$.ordered <- is.ordered(val)
      invisible(self)
    },
    asFactor = function() {
      factor(
        self$value(),
        levels  = private$.levels,
        ordered = private$.ordered
      )
    },
    levels = function() {
      private$.levels
    },
    ordered = function() {
      if (is.null(private$.ordered)) return(FALSE)
      return(private$.ordered)
    },
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
RSFactor$isNAClass  <- FALSE




#' @title Factory Method for RSFactor
#'
#' @description
#' Constructs an `RSFactor` from a valid R factor scalar.
#' If the input is `NA`, an `RSNAFactor` is returned instead.
#'
#' @param val A factor value of length 1.
#' @return An instance of `RSFactor` or `RSNAFactor`.
#'
#' @examples
#' RSFactor$factorWith(factor("A"))
#' RSFactor$factorWith(factor(NA_character_))  # returns RSNAFactor
#'
#' @export
RSFactor$factorWith <- function(val) {
  if (is.na(val)) return(RSNAFactor$new())
  return(RSFactor$new(val))
}



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
#' Normally, this class is constructed internally via `RSFactor$factorWith()`.
#'
#' @examples
#' na_f <- RSFactor$factorWith(factor(NA_character_))
#' na_f$ordered()  # FALSE
#' na_f$levels()   # NULL
#'
#' @export
RSNAFactor <- R6::R6Class(
  "RSNAFactor",
  cloneable = FALSE,
  inherit = RSFactor,
  public = list(
    initialize = function() {
      super$initialize(factor(NA_character_))
      private$.levels <- NULL
      private$.ordered <- FALSE
      invisible(self)
    }
  )
)
RSNAFactor$isNAClass <- TRUE
