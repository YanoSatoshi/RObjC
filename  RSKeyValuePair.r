#' RSKeyValuePair
RSKeyValuePair <- R6::R6Class(
  ### class name ###
  "RSKeyValuePair",
  cloneable = FALSE,
  inherit   = RSObject,
  public    = list(
    initialize = function(key, value) {
      super$initialize()
      stopifnot(!is.null(key$isKindOf) && key$isKindOf(RSObject))
      stopifnot(!is.null(value$isKindOf) && value$isKindOf(RSObject))
      private$.key   <- key
      private$.value <- value
      invisible(self)
    },
    key = function() {
      private$.key
    },
    value = function() {
      private$.value
    },
    isEqual = function(other) {
      if (!other$isMemberOf(self$class())) return(FALSE)
      isTRUE(private$.key$isEqual(other$key()))
    },
    hash = function(){
      private$.key$hash()
    }
  ),
  private   = list(
    .key    = NULL,
    .value  = NULL
  )
)
