RSMutableCopying <- R6::R6Class(
  ### protocol name ###
  "RSMutableCopying",
  public = list(
    requiredMethods = function() {
      c("mutableCopy")
    },
    mutableCopyObjectWithENvir = function(copying_obj, envir) {
      if (!copying_obj$respondsTo("mutableCopyWithEnvir")) {
        stop(
          sprintf(
            "Object of class %s does not implement mutableCopyWithEnvir().",
            copying_obj$class()$classname
          )
        )
      }
    }
  )
)