#' @include runtime.r
#' @title RSProtocol
#' @description TODO: document the root protocol generator.
#' @noRd
#' @export
RSProtocol <- R6::R6Class(
  "RSProtocol",
  class     = FALSE,
  cloneable = FALSE,
  portable  = TRUE,
  active    = NULL,
  inherit   = NULL,
  public    = list(
    initialize = function() {
      # DO NOT OVERRIDE
      class(self) <- vapply(
        robjc_inheritChain(private$.__isa),
        function(klass) klass$classname,
        character(1)
      )
      invisible(self) 
    }
  ),
  private   = list(
    .__isa = NULL
  )
)
