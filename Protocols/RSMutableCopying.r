RSMutableCopying <- R6::R6Class(
  ### protocol name ###
  "RSMutableCopying",
  inherit = RSProtocol,
  public = list(
    requiredMethods = function() {
      c("mutableCopy")
    },
    mutableCopyObjectWithEnvir = function(copying_obj, envir) {
      if (is.null(copying_obj$respondsTo)) {
        stop("Protocol cannot be implimented other than RSObject and its inheritants.") # nolint
      }
      if (!copying_obj$respondsTo("mutableCopyWithEnvir")) {
        stop(sprintf(
          "Object of class %s does not implement mutableCopyWithEnvir().",
          copying_obj$class()$classname
        ))
      }
    }
  )
)
register_rs_protocol(RSMutableCopying, RSMutableCopying$new())