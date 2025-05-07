#' @title RSDataFrame: Immutable Data Frame Object
#'
#' @description
#' `RSDataFrame` is an immutable subclass of `RSMutableDataFrame`. Its data structure and key columns
#' are fixed upon creation. Any attempt to modify the data or structure will raise an error.
#' It is intended for safe and consistent use in hash-based containers and comparisons.
#'
#' @note Use `copyWithEnv()` or `mutableCopyWithEnv()` to safely duplicate or convert.
#'
#' @export
RSDataFrame <- R6::R6Class(
  "RSDataFrame",
  inherit = RSMutableDataFrame,
  cloneable = FALSE,
  public = list(
#' @describeIn RSDataFrame
#' This method is disabled for `RSDataFrame` as it is immutable.
#' Use `RSMutableDataFrame` instead.
    setData = function(df) {
      stop("RSDataFrame is immutable. Use RSMutableDataFrame instead.")
    },



#' @describeIn RSDataFrame
#' This method is disabled for `RSDataFrame` as it is immutable.
#' Use `RSMutableDataFrame` instead.
    setColumn = function(name, values) {
      stop("RSDataFrame is immutable. Use RSMutableDataFrame instead.")
    },



#' @describeIn RSDataFrame
#' Returns an identical immutable copy of the current object placed into the specified environment.
#' Because the object is already immutable, this method typically returns `self` unless an environment
#' switch is needed for structural consistency.
#'
#' @param env The target environment.
#' @return A copy of the current immutable object, typically `self`.
    copyWithEnv = function(env = parent.frame()) {
      if (identical(env, environment(self))) return(self)
      do.call(
        RSDataFrame$new,
        list(df = private$.df, key_columns = private$.key_columns),
        envir = env
      )
    },



#' @describeIn RSDataFrame
#' Returns a new mutable version of the current object in the specified environment.
#' The content is duplicated to allow modifications, and the result is typically
#' an instance of the corresponding mutable subclass (e.g., `RSMutableArray` for `RSArray`).
#'
#' @param env The environment in which the new mutable object will be evaluated.
#' @return A new mutable copy of the object.
    mutableCopyWithEnv = function(env = parent.frame()) {
      do.call(
        RSMutableDataFrame$new,
        list(df = private$.df, key_columns = private$.key_columns),
        envir = env
      )
    }
  )
)
