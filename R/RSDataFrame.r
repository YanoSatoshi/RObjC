#' @include RSMutableDataFrame.r
#' @title RSDataFrame: Immutable Data Frame Object
#'
#' @description
#' `RSDataFrame` is an immutable subclass of `RSMutableDataFrame`. Its data structure and key columns
#' are fixed upon creation. Any attempt to modify the data or structure will raise an error.
#' It is intended for safe and consistent use in hash-based containers and comparisons.
#'
#' @note Use `copy()` or `mutableCopy()` to safely duplicate or convert.
#'
#' @export
RSDataFrame <- RSObjectClass(
  "RSDataFrame",
  version = "0.0.2 alpha",
  super = RSMutableDataFrame,
  protocols = list(RSCopying, RSMutableCopying),
  public = list(

    #' @description
    #' This method is disabled for `RSDataFrame` as it is immutable.
    #' Use `RSMutableDataFrame` instead.
    #' 
    #' @param df Parameters that would normally be used in the mutable version of this method.
    setData = function(df) {
      stop("RSDataFrame is immutable. Use RSMutableDataFrame instead.")
    },



    #' @description
    #' This method is disabled for `RSDataFrame` as it is immutable.
    #' Use `RSMutableDataFrame` instead.
    #' 
    #' @param name Parameters that would normally be used in the mutable version of this method.
    #' @param values Parameters that would normally be used in the mutable version of this method.
    setColumn = function(name, values) {
      stop("RSDataFrame is immutable. Use RSMutableDataFrame instead.")
    },



    #' @description
    #' Returns the object itself, since `RSDataFrame` is immutable.
    #'
    #' This is a no-op copy: the method returns `self`, as immutable
    #' objects are safe to reuse without duplication.
    #'
    #' @return The same `RSDataFrame` instance (`self`).
    copy = function() return(self),



    #' @description
    #' Returns a mutable version of the current data frame.
    #'
    #' This creates a new `RSMutableDataFrame` instance that wraps the same underlying tibble,
    #' allowing modifications to the data. The copy is shallow: the underlying data is reused.
    #'
    #' This method is typically used to transition from an immutable to a mutable form
    #' when updates are needed.
    #'
    #' @return A new `RSMutableDataFrame` instance.
    mutableCopy = function() RSMutableDataFrame$new(df = private$.df, key_columns = private$.key_columns)
  )
)
