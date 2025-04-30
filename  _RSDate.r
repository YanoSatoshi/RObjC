#' RSDate
RSDate <- R6::R6Class(
  ### class name ###
  "RSDate",
  cloneable = FALSE,
  inherit = RSScalar,
  public = list(
    initialize = function(val = Sys.Date()) {
      if (!inherits(val, "Date"))   stop("val must be of class Date")
      super$initialize(val)
      invisible(self)
    },
    isEqual = function(other) {
      if (!other$isMemberOf(self$class())) return(FALSE)
      private$.val == other$value()
    },
    hash = function() {
      as.integer(as.numeric(private$.val))
    }
  )
)
RSDate$isNAClass <- FALSE
RSDate$dateWith  <- function(val) {
  if (is.na(val)) return(RSNADate$new())
  return(RSDate$new(val))
}


RSNADate <- R6::R6Class(
  "RSDate",
  cloneable = FALSE,
  inherit = RSDate,
  public = list(
    initialize = function() {
      super$initialize(NA_Date_)
      invisible(self)
    }
  )
)
RSNADate$isNAClass <- TRUE
