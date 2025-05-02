RSArray <- R6::R6Class(
  "RSArray",
  inherit = RSMutableArray,
  cloneable = FALSE,
  public = list(
    copyWithEnv = function(env = parent.frame()) {
      if (identical(env, environment(self))) return(self)
      do.call(
        RSArray$new,
        list(
          objects = private$.storage,
          hash_operator = private$.hash_operator
        ),
        envir = env
      )
    },
    mutableCopyWithEnv = function(env = parent.frame()) {
      do.call(
        RSMutableArray$new,
        list(
          objects = private$.storage,
          hash_operator = private$.hash_operator
        ),
        envir = env
      )
    },
    ## deactivate mutative methods ##
    addObject = function(obj) {
      stop(msg_immutable_try_mutate(self$class()))
    },
    insertObjectAtIndex = function(obj, idx) {
      stop(msg_immutable_try_mutate(self$class()))
    },
    replaceObjectAtIndex = function(obj, idx) {
      stop(msg_immutable_try_mutate(self$class()))
    },
    removeObjectAtIndex = function(obj, idx) {
      stop(msg_immutable_try_mutate(self$class()))
    },
    removeAllObjects = function(obj) {
      stop(msg_immutable_try_mutate(self$class()))
    }
  )
)
