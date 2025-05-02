#' RSBoolean
RSBoolean <- R6::R6Class(
  "RSBoolean",
  cloneable = FALSE,
  inherit = RSScalar,
  public = list(
    initialize = function(val) {
      if (!inherits(val, "logical")) stop("val must be of class logical")
      super$initialize(val)
      invisible(self)
    },
    compare = function(other, na.last = TRUE) {
      if (!private$.comparable(other)) {
        stop(sprintf(
          "Abort trying to compare %s and %s",
          self$class()$classname, other$class()$classname
        ))
      }
      na_val <- if (na.last) .Machine$double.xmax else .Machine$double.xmin
      val1 <- as.numeric(self$value())
      val2 <- as.numeric(other$value())
      val1 <- if (is.na(val1)) na_val else val1
      val2 <- if (is.na(val2)) na_val else val2
      return(sign(val1 - val2))
    },
    ## CAST TO RSNumber
    asNumber = function() {
      RSNumber$new(as.numeric(private$.val))
    }
  )
)
RSBoolean$isNAClass <- FALSE
RSBoolean$boolWith  <- function(val) {
  if (is.na(val)) return(RSNABoolean$new())
  return(RSBoolean$new(as.logical(val)))
}


#' NABoolean
RSNABoolean <- R6::R6Class(
  "RSNABoolean",
  cloneable = FALSE,
  inherit = RSBoolean,
  public = list(
    initialize = function() {
      super$initialize(NA) # NA は素で logical
      invisible(self)
    }
  )
)
RSNABoolean$isNAClass <- TRUE
