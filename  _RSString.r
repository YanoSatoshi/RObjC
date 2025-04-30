#' RSString
RSString <- R6::R6Class(
  "RSString",
  cloneable = FALSE,
  inherit = RSScalar,
  public = list(
    initialize = function(val) {
      if (!inherits(val, c("character", "factor"))) {
        stop("RSString can wrap character or factor values.")
      }
      super$initialize(val)
      invisible(self)
    }
  )
)
RSString$isNAClass  <- FALSE
RSString$stringWith <- function(val) {
  if (is.na(val)) return(RSNAString$new())
  return(RSString$new(val))
}


RSNAString <- R6::R6Class(
  "NARSString",
  cloneable = FALSE,
  inherit = RSString,
  public = list(
    initialize = function() {
      super$initialize(NA_character_)
      invisible(self)
    }
  )
)
RSNAString$isNAClass <- TRUE
