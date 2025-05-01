RSCopying <- R6::R6Class(
  ### protocol name ###
  "RSCopying",
  inherit = RSProtocol,
  cloneable = FALSE,
  public = list(
    requiredMethods = function() {
      c("copyWithEnvir(envir)")
    },
    copyObjectWithEnvir = function(copying_obj, envir) {
      if (!copying_obj$respondsTo("copyWithEnvir")) {
        stop(
          sprintf(
            "Object of class %s does not implement copyWithEnvir().",
            copying_obj$class()$classname
          )
        )
      }
      copying_obj$copyWithEnvir(envir)
    }
  )
)