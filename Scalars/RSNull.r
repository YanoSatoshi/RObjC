#' RSNull
RSNull <- R6::R6Class(
  "RSNull",
  cloneable = FALSE,
  inherit = RSScalar,
  public = list(
    initialize = function() {
      if (!is.null(RSNull$instance)) {
        stop("Use RSNull$null() instead of new().")
      }
      super$initialize(0L)
      private$.val <- NULL
      RSNull$instance <<- self
      self$lockVariables()
      lockBinding("instance", RSNull)
      invisible(self)
    },
    isEqual = function(other) {
      self$isMemberOf(other$class())
    },
    hash = function() {
      0L  # NULLは常に同じハッシュ
    },
    value = function() {
      NULL
    },
    # Locks all current bindings in the object to prevent accidental additions or removals.
    lockVariables = function() {
      for (name in ls(self, all.names = TRUE)) lockBinding(name, self)
      lockEnvironment(as.environment(self), bindings = TRUE)
    }
  )
)
RSNull$instance  <- NULL
RSNull$isNAClass <- NA
RSNull$null <- function() {
  if (is.null(RSNull$instance)) return(RSNull$new())
  RSNull$instance
}
