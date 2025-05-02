RSMutableDataFrame <- R6::R6Class(
  "RSMutableDataFrame",
  inherit = RSObject,
  cloneable = FALSE,
  private = list(
    .df = NULL,
    .key_columns = character(),
    .checkDataFrame = function(df) {
      if (!requireNamespace("tibble", quietly = TRUE)) {
        stop("Package 'tibble' is required.")
      }
      tibble::as_tibble(df)
    },
    .validateKeys = function() {
      if (!all(private$.key_columns %in% colnames(private$.df))) {
        stop("Invalid key_columns: some columns do not exist in the data frame.") # nolint
      }
    }
  ),
  public = list(
    initialize = function(df = data.frame(), key_columns = character()) {
      super$initialize()
      private$.df <- private$.checkDataFrame(df)
      if (length(key_columns) == 0) {
        key_columns <- names(Filter(is.atomic, private$.df))
      }
      private$.key_columns <- key_columns
    },
    getData = function() private$.df,
    setData = function(df) {
      private$.df <- private$.checkDataFrame(df)
    },
    getColumn = function(name) private$.df[[name]],
    setColumn = function(name, values) private$.df[[name]] <- values,
    nrow = function() nrow(private$.df),
    ncol = function() ncol(private$.df),
    colnames = function() colnames(private$.df),
    rownames = function() rownames(private$.df),
    keyColumns = function() private$.key_columns,
    isEqual = function(other) {
      if (is.null(other) || !other$isKindOf(RSMutableDataFrame)) return(FALSE)
      private$.validateKeys()
      df1 <- private$.df
      df2 <- other$getData()
      if (!identical(colnames(df1), colnames(df2))) return(FALSE)
      if (nrow(df1) != nrow(df2)) return(FALSE)
      identical(all.equal(df1, df2, check.attributes = TRUE), TRUE)
    },
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
    copyWithEnv = function(env = parent.frame()) {
      do.call(
        RSDataFrame$new,
        list(df = private$.df, key_columns = private$.key_columns),
        envir = env
      )
    },
    mutableCopyWithEnv = function(env = parent.frame()) {
      if (identical(env, environment(self))) return(self)
      do.call(
        RSMutableDataFrame$new,
        list(df = private$.df, key_columns = private$.key_columns),
        envir = env
      )
    }
  )
)
