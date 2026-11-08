#' @importFrom tibble tibble
#' @include api_setup.r
#' @include RSMutableCopying.r
#' @include RSCopying.r
#' @include RSEncoding.r
#'
#' @title RSDataFrame: Immutable Data Frame Wrapper
#'
#' @description
#' `RSDataFrame` wraps a data frame as a tibble. It provides read access to its
#' columns and dimensions, exact equality through `identical()` on the stored
#' tibble, and a coarse hash based on row count, column names, and storage types.
#' Different data frames with the same structure can have the same hash.
#' Use a custom class when content-sensitive hashing is needed.
#'
#' `RSMutableDataFrame` extends this class with methods to change the table.
#' `copy()` returns the immutable receiver, while `mutableCopy()` creates a
#' mutable wrapper around the same data.
#'
#' @section Protocols:
#' - [RSCopying]
#' - [RSMutableCopying]
#' - [RSCoding]
#'
#' @examples
#' people <- data.frame(name = c("Ada", "Ben"), score = c(8L, 9L),
#'                      stringsAsFactors = FALSE)
#' table <- RSDataFrame$new(people)
#' table$getColumn("name")
#' table$nrow()
#' editable <- table$mutableCopy()
#' editable$setColumn("score", c(10L, 9L))
#' table$getColumn("score")  # The immutable receiver still has its original data.
#'
#' @export
RSDataFrame <- RSObjectClass(
  "RSDataFrame",
  version = "0.2.0",
  super_class = RSObject,
  protocols = list(RSCopying, RSMutableCopying, RSCoding),
  private = list(
    .df = NULL,
    .checkDataFrame = function(df) {
      if (!requireNamespace("tibble", quietly = TRUE))
        stop("Package 'tibble' is required.")
      tibble::as_tibble(df)
    }
  ),
  public = list(
    #' @description Initializes the object with a data frame, stored as a tibble.
    #' @param df A `data.frame` or `tibble`.
    initialize = function(df = data.frame()) {
      super$initialize()
      private$.df <- private$.checkDataFrame(df)
      invisible(self)
    },



    #' @description Encodes the stored tibble as a base R serialization value.
    #' @param coder An [RSKeyedArchiver] recording keyed values.
    #' @return `self`, invisibly.
    encodeWithCoder = function(coder) {
      coder$encodeObjectForKey(private$.df, "data")
      invisible(self)
    },



    #' @description Restores the tibble without calling `initialize()`.
    #'   This also serves [RSMutableDataFrame]. Legacy archives may contain an
    #'   unused `key_columns` field.
    #' @param coder An [RSKeyedUnarchiver] reading keyed values.
    #' @return `self`, invisibly.
    initWithCoder = function(coder) {
      df <- coder$decodeObjectForKey("data")
      if (!is.data.frame(df))
        stop("Decoded data must be a data frame.", call. = FALSE)
      private$.df <- private$.checkDataFrame(df)
      invisible(self)
    },



    #' @description Returns the internal tibble object.
    #' @return A `tibble::tibble` object.
    getDataFrame = function() private$.df,



    #' @description Returns the values stored in the specified column.
    #' @param name Column name.
    #' @return A vector of column values.
    getColumn = function(name) private$.df[[name]],



    #' @description Returns the number of rows in the data frame.
    #' @return Integer.
    nrow = function() nrow(private$.df),



    #' @description Returns the number of columns.
    #' @return Integer.
    ncol = function() ncol(private$.df),



    #' @description Returns the column names of the data frame.
    #' @return Character vector.
    colnames = function() colnames(private$.df),



    #' @description Returns the row names of the data frame.
    #' @return Character vector.
    rownames = function() rownames(private$.df),



    #' @description Compares the complete stored tibble with another data frame
    #'   wrapper using `identical()`.
    #' @param other Another `RSDataFrame` or `RSMutableDataFrame`.
    #' @return `TRUE` if equal; `FALSE` for unequal or incompatible inputs.
    isEqual = function(other) {
      if (!robjc_isInstance(other) || !other$isKindOf(RSDataFrame))
        return(FALSE)
      identical(private$.df, other$getDataFrame())
    },



    #' @description Computes a coarse hash from row count, column names, and
    #'   column storage types. Cell values and other attributes are not included,
    #'   so collisions between unequal data frames are expected. The final
    #'   result passes through [robjc_normalize_hash()].
    #' @return One non-missing integer hash value.
    hash = function() {
      df <- private$.df
      seed <- c(
        nrow(df),
        paste(
          colnames(df),
          vapply(df, typeof, character(1)),
          sep = "/"
        )
      )
      hashes <- vapply(
        seed,
        RSHashOperator$instance$computeHashCodeWithString,
        integer(1)
      )
      robjc_normalize_hash(Reduce(bitwXor, hashes, init = 0L))
    },



    #' @description Returns the immutable receiver without duplicating it.
    #' @return The same `RSDataFrame` instance.
    copy = function() self,



    #' @description Creates a mutable wrapper around the same tibble. R's
    #'   copy-on-modify semantics separate the stored data on later edits.
    #' @return A new [RSMutableDataFrame] instance.
    mutableCopy = function() RSMutableDataFrame$new(df = private$.df)
  )
)
