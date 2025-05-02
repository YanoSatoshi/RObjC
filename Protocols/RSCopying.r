RSCopying <- R6::R6Class(
  ### protocol name ###
  "RSCopying",
  inherit = RSProtocol,
  cloneable = FALSE,
  public = list(
    requiredMethods = function() {
      c("copyWithEnv")
    },
    copyObjectWithEnv = function(copying_obj, env) {
      if (is.null(copying_obj$respondsTo)) {
        stop("Protocol cannot be implimented other than RSObject and its inheritants.") # nolint
      }
      if (!copying_obj$respondsTo("copyWithEnv")) {
        stop(sprintf(
          "Object of class %s does not implement copyWithEnv().",
          copying_obj$class()$classname
        ))
      }
      copying_obj$copyWithEnv(env)
    }
  )
)
register_rs_protocol(RSCopying, RSCopying$new())