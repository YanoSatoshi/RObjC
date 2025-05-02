#' RSDateTime
RSDateTime <- R6::R6Class(
  ### class name ###
  "RSDateTime",
  cloneable = FALSE,
  inherit = RSScalar,
  public = list(
    initialize = function(val = Sys.time()) {
      if (!inherits(val, "POSIXct")) stop("val must be of class POSIXct")
      # タイムゾーンを固定
      val <- as.POSIXct(val, tz = "Asia/Tokyo")
      super$initialize(val)
      invisible(self)
    },
    hash = function() {
      # UNIX時間（秒単位）を整数に変換してハッシュに
      as.integer(as.numeric(private$.val))
    }
  )
)
RSDateTime$isNAClass <- FALSE
RSDateTime$dateTimeWith <- function(val) {
  if (is.na(val)) return(RSNADateTime$new())
  return(RSDateTime$new(val))
}


RSNADateTime <- R6::R6Class(
  "RSNADateTime",
  cloneable = FALSE,
  inherit = RSDateTime,
  public = list(
    initialize = function() {
      super$initialize(NA_POSIXct_)
      invisible(self)
    }
  )
)
RSNADateTime$isNAClass <- TRUE
