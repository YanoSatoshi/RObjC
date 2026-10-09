#' @include RSScalar.r
#' @title RSBoolean: Logical Scalar Wrapper
#'
#' @description
#' `RSBoolean` wraps a logical (TRUE/FALSE) scalar. It inherits from `RSScalar` and
#' provides boolean-specific behavior, including conversion to numeric via `asNumber()`.
#' It rejects NA values; use `RSNABoolean` for NA.
#' 
#' @section Methods:
#' - `asNumber()` — Returns an `RSNumber` with value 1 or 0.
#' 
#' @section Usage:
#' To create an instance, use the static constructor:
#' \code{RSBoolean_withBoolean(val)}, which returns \code{RSNABoolean} if \code{val} is \code{NA}.
#' 
#' @export
RSBoolean <- RSScalarClass(
  "RSBoolean",
  version = "0.0.2 alpha",
  super_class = RSScalar,
  is_na_class = FALSE,
  public = list(

    #' @description
    #' Initializes an `RSBoolean` object by wrapping a logical scalar.
    #' This constructor verifies that the input is of class `logical`, and stores
    #' it as the internal value. Only scalar (length 1) logical values are supported.
    #' @param val A logical scalar to be wrapped.
    #' @return The initialized `RSBoolean` object (invisible).
    initialize = function(val) {
      if (!inherits(val, "logical")) stop("val must be of class logical")
      super$initialize(val)
      invisible(self)
    },

    #' @description
    #' Returns a numeric representation of the boolean value.
    #' Converts the internal logical value (`TRUE` or `FALSE`) to an `RSNumber`
    #' object (`1` or `0`, respectively).
    #' @return An `RSNumber` object with value `1` (for `TRUE`) or `0` (for `FALSE`).
    asNumber = function() {
      RSNumber$new(as.numeric(private$.val))
    }

  )
)


#' @title RSNABoolean: Wrapper for NA-logical Scalar
#'
#' @description `RSNABoolean` is a subclass of `RSBoolean` that safely represents `NA` logical values.
#' This class is typically used internally by factory methods.
#'
#' @note
#' Use `RSBoolean_withBoolean(val)` instead of instantiating directly.
#'
#' @export
RSNABoolean <- RSScalarClass(
  "RSNABoolean",
  version = "0.0.2 alpha",
  super_class = RSBoolean,
  is_na_class = TRUE,
  public = list(

    #' @description
    #' Initializes an `RSNABoolean` object that wraps an `NA` logical value.
    #' This class represents the NA counterpart to `RSBoolean`, and should be used
    #' whenever a logical `NA` is required within the scalar object framework.
    #' Internally, it delegates to `RSBoolean$initialize(NA)` and returns the object invisibly.
    initialize = function() {
      super$initialize(NA) # NA は素で logical
      invisible(self)
    }
  )
)
