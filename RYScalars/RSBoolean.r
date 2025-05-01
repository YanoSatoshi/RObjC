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
