RSDataFrame <- R6::R6Class(
  "RSDataFrame",
  inherit = RSMutableDataFrame,
  cloneable = FALSE,
  public = list(
    # override setters to disable
    setData = function(df) {
      stop("RSDataFrame is immutable. Use RSMutableDataFrame instead.")
    },
    setColumn = function(name, values) {
      stop("RSDataFrame is immutable. Use RSMutableDataFrame instead.")
    },
    # override copy to ensure immutability
    copyWithEnv = function(env = parent.frame()) {
      if (identical(env, environment(self))) return(self)
      do.call(
        RSDataFrame$new,
        list(df = private$.df, key_columns = private$.key_columns),
        envir = env
      )
    },
    mutableCopyWithEnv = function(env = parent.frame()) {
      do.call(
        RSMutableDataFrame$new,
        list(df = private$.df, key_columns = private$.key_columns),
        envir = env
      )
    }
  )
)
