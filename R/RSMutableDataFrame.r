#' @include RSDataFrame.r
#'
#' @title RSMutableDataFrame: Mutable Data Frame Wrapper
#'
#' @description
#' `RSMutableDataFrame` extends [RSDataFrame] with methods to replace its stored
#' tibble or a column. It inherits read access, exact equality, coarse
#' structure-based hashing, and keyed archive support from its parent.
#'
#' `copy()` creates an immutable [RSDataFrame]; `mutableCopy()` is inherited
#' and creates a separate mutable wrapper.
#'
#' @section Protocols:
#' - [RSCopying]
#' - [RSMutableCopying]
#' - [RSCoding]
#'
#' @examples
#' editable <- RSMutableDataFrame$new(
#'   data.frame(name = c("Ada", "Ben"), stringsAsFactors = FALSE)
#' )
#' snapshot <- editable$copy()
#' editable$setColumn("name", c("Ada", "Bea"))
#' editable$getColumn("name")
#' snapshot$getColumn("name")  # The immutable copy retains the earlier values.
#'
#' @export
RSMutableDataFrame <- RSObjectClass(
  "RSMutableDataFrame",
  version = "0.2.0",
  super_class = RSDataFrame,
  protocols = list(RSCopying, RSMutableCopying, RSCoding),
  public = list(
    #' @description Replaces the entire data frame, converting it to a tibble.
    #' @param df A `data.frame` or `tibble`.
    setDataFrame = function(df) private$.df <- private$.checkDataFrame(df),



    #' @description Replaces a column's contents with new values.
    #' @param name Column name.
    #' @param values A vector of replacement values.
    setColumn = function(name, values) private$.df[[name]] <- values,



    #' @description Returns a new immutable wrapper around the same tibble.
    #'   R's copy-on-modify semantics separate the stored data on later edits.
    #' @return A new [RSDataFrame] instance.
    copy = function() RSDataFrame$new(df = private$.df)
  )
)
