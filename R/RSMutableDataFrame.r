#' @importFrom tibble tibble
#' @include api_setup.r
#' @include RSMutableCopying.r
#' @include RSCopying.r
#' 
#' @title RSMutableDataFrame: Mutable Data Frame Object with Key Column Semantics
#'
#' @description
#' `RSMutableDataFrame` wraps a `tibble::tibble` as an `RSObject`, providing controlled access
#' to its columns, rows, and key structure. It allows for safe manipulation and hashing of
#' tabular data within the RS framework, especially in environments that require
#' immutable tracking or serialization of row-keyed structures.
#'
#' The internal data frame is always stored as a `tibble`, and key columns are used for
#' hashing and equality comparisons.
#'
#' @section Core Features:
#' - Column access and mutation
#' - Customizable key columns
#' - Structural equality and hash support
#' - Conversion to immutable `RSDataFrame`
#'
#' @export
RSMutableDataFrame <- RSObjectClass(
  "RSMutableDataFrame",
  super_class = RSObject,
  protocols = list(RSMutableCopying, RSCopying),
  private = list(
    .df = NULL,
    .key_columns = character(),
    .checkDataFrame = function(df) {
      if (!requireNamespace("tibble", quietly = TRUE))
        stop("Package 'tibble' is required.")
      tibble::as_tibble(df)
    },
    .validateKeys = function() {
      if (!all(private$.key_columns %in% colnames(private$.df)))
        stop("Invalid key_columns: some columns do not exist in the data frame.") # nolint
    }
  ),
  public = list(

#' @description
#' Initializes the object with a tibble and a vector of key column names.
#' If `key_columns` is empty, all atomic columns are selected as keys.
#'
#' @param df A `data.frame` or `tibble`.
#' @param key_columns Character vector of key column names.
    initialize = function(df = data.frame(), key_columns = character()) {
      super$initialize()
      private$.df <- private$.checkDataFrame(df)
      if (length(key_columns) == 0) {
        key_columns <- names(Filter(is.atomic, private$.df))
      }
      private$.key_columns <- key_columns
    },



#' @description
#' Returns the internal tibble object.
#'
#' @return A `tibble::tibble` object.
    getData = function() private$.df,



#' @description
#' Replaces the entire data frame. The input is converted to a tibble.
#'
#' @param df A `data.frame` or `tibble`.
    setData = function(df) {
      private$.df <- private$.checkDataFrame(df)
    },



#' @description
#' Returns the values stored in the specified column.
#'
#' @param name Column name.
#' @return A vector of column values.
    getColumn = function(name) private$.df[[name]],



#' @description
#' Replaces a column's contents with a new vector of values.
#'
#' @param name Column name.
#' @param values A vector of replacement values.
    setColumn = function(name, values) private$.df[[name]] <- values,



#' @description
#' Returns the number of rows in the data frame.
#' @return Integer.
    nrow = function() nrow(private$.df),



#' @description
#' Returns the number of columns.
#' @return Integer.
    ncol = function() ncol(private$.df),



#' @description
#' Returns the column names of the data frame.
#' @return Character vector.
    colnames = function() colnames(private$.df),



#' @description
#' Returns the row names of the data frame.
#' @return Character vector.
    rownames = function() rownames(private$.df),



#' @description
#' Returns the names of the key columns used for hashing and comparison.
#' @return Character vector.
    keyColumns = function() private$.key_columns,



#' @description
#' Compares this object to another `RSMutableDataFrame`.
#' The comparison checks column names, row count, and full data equality including attributes.
#'
#' @param other Another `RSMutableDataFrame`.
#' @return `TRUE` if equal; otherwise `FALSE`.
    isEqual = function(other) {
      if (robjc_isRobjcObject(other) || !other$isKindOf(RSMutableDataFrame)) return(FALSE)
      private$.validateKeys()
      df1 <- private$.df
      df2 <- other$getData()
      if (!identical(colnames(df1), colnames(df2))) return(FALSE)
      if (nrow(df1) != nrow(df2)) return(FALSE)
      identical(all.equal(df1, df2, check.attributes = TRUE), TRUE)
    },



#' @description
#' Computes a hash based on the contents of the key columns and metadata such as row count and column names.
#' @return Integer hash value.
    hash = function() {
      private$.validateKeys()
      df <- private$.df
      keys <- private$.key_columns
      key_data <- df[, keys, drop = FALSE]
      key_strings <- apply(
        key_data, 1, function(row) paste(row, collapse = "|")
      )
      hash_basis <- paste(
        c(colnames(df), nrow(df), key_strings), collapse = "::"
      )
      RSHashOperator$computeHashCodeWithString(hash_basis)
    },



    #' @description
    #' Returns an immutable copy of the data frame as an `RSDataFrame`.
    #' 
    #' This performs a shallow copy: the underlying tibble is shared,
    #' while the wrapper class is replaced with its immutable counterpart.
    #'
    #' This method is typically used to convert a mutable data frame to its immutable form.
    #'
    #' @return A new `RSDataFrame` instance.
    copy = function() {
      RSDataFrame$new(
        df = private$.df,
        key_columns = private$.key_columns
      )
    },



    #' @description
    #' Returns a mutable copy of the data frame as an `RSMutableDataFrame`.
    #' 
    #' This performs a shallow copy: the underlying tibble is reused,
    #' while the wrapper is recreated. If the original is already mutable,
    #' returning `self` would be acceptable depending on context.
    #'
    #' @return A new `RSMutableDataFrame` instance.
    mutableCopy = function() {
      RSMutableDataFrame$new(
        df = private$.df,
        key_columns = private$.key_columns
      )
    }
  )
)
