#' RSFactor
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
RSFactor$factorWith <- function(val) {
  if (is.na(val)) return(RSNAFactor$new())
  return(RSFactor$new(val))
}



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
