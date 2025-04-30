#' RSNumber
RSNumber <- R6::R6Class(
  "RSNumber",
  cloneable = FALSE,
  inherit = RSScalar,
  public = list(
    initialize = function(val) {
      if (!inherits(val, "numeric")) stop("val must be of class numeric")
      super$initialize(val)
      invisible(self)
    }
  )
)
RSNumber$isNAClass  <- FALSE
RSNumber$numberWith <- function(val) {
  if (is.na(val)) return(RSNANumber$new())
  return(RSNumber$new(val))
}

RSNANumber <- R6::R6Class(
  "RSNANumber",
  cloneable = FALSE,
  inherit = RSNumber,
  public = list(
    initialize = function() {
      super$initialize(NA_real_)
      invisible(self)
    }
  )
)
RSNANumber$isNAClass <- TRUE
