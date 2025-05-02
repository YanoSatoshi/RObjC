# REMOVE ON RELEASE
if (exists('register_rs_protocol')) unlockBinding("register_rs_protocol", environment()) # nolint
warning("Reset 'register_rs_protocol'")

register_rs_protocol <- function(klass, obj) {
  klass$.singleton <- obj
  klass$protocol <- function() klass$.singleton
  obj$lockVariables()
  lockBinding(".singleton", klass)
  invisible(obj)
}
lockBinding("register_rs_protocol", environment())
RSProtocol <- R6::R6Class(
  ### protocol name ###
  "RSProtocol",
  inherit = RSObject,
  cloneable = FALSE,
  public = list(
    initialize = function() {
      super$initialize()
      klass <- self$class()
      if (!is.null(klass$.singleton)) {
        stop(sprintf("Use %s$protocol() instead of new().", klass$classname))
      }
    },
    requiredMethods = function() {
      warning(
        sprintf(
          "'requiredMethods' not over-ridden yet for %s.",
          self$class()$classname
        )
      )
      character(0)
    },
    # Locks all current bindings in the object to prevent accidental additions or removals. #nolint
    lockVariables = function() {
      for (name in ls(self, all.names = TRUE)) lockBinding(name, self)
      lockEnvironment(as.environment(self), bindings = TRUE)
    }
  )
)
register_rs_protocol(RSProtocol, RSProtocol$new())