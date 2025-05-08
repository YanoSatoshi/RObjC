#' @title RSString: Character or Factor Scalar Wrapper
#'
#' @description
#' `RSString` is a scalar wrapper class that encapsulates character or factor values.
#' It inherits from `RSScalar` and provides support for equality comparison,
#' hashing, and sorting. This class allows factor levels to be treated interchangeably
#' with character strings at the scalar abstraction level.
#'
#' The constructor accepts a single-length character or factor value.
#' Use the factory method `RSString$stringWith()` to construct an instance safely,
#' especially when the input may be `NA`.
#'
#' @section See Also:
#' [RSNAString] for representing `NA_character_` values in a class-based form.
#'
#' @section Usage:
#' Use \code{RSString$stringWith(val)} to safely construct from a character or factor.
#' If \code{val} is \code{NA}, it returns \code{RSNAString}.
#' 
#' @examples
#' s <- RSString$stringWith("hello")
#' s$value()  # "hello"
#'
#' @export
RSString <- R6::R6Class(
  "RSString",
  cloneable = FALSE,
  inherit = RSScalar,
  public = list(

    #' @description
    #' Initializes an `RSString` object by wrapping a scalar character or factor value.
    #' The input must be of class `"character"` or `"factor"` and of length 1.
    #' If the value is `NA`, use `RSNAString` instead.
    #'
    #' Internally, the value is stored as-is and handled by the parent `RSScalar` class.
    #'
    #' @param val A scalar character or factor value. Must not be `NA`.
    #'
    #' @return An initialized `RSString` object (invisible).
    initialize = function(val) {
      if (!inherits(val, c("character", "factor"))) {
        stop("RSString can wrap character or factor values.")
      }
      super$initialize(val)
      invisible(self)
    }
  )
)
RSString$isNAClass  <- FALSE





#' @title RSNAString: Wrapper for NA-character Scalar
#'
#' @description
#' `RSNAString` is a subclass of `RSString` used specifically to represent `NA_character_`
#' values. This class conforms to the `RSScalar` interface while clearly indicating
#' that it wraps a missing string value.
#'
#' This class is typically instantiated internally via `RSString$stringWith()`.
#' The `isNAClass` flag is set to TRUE to enable differentiated behavior in comparison logic.
#'
#' @examples
#' na_str <- RSString$stringWith(NA_character_)
#' na_str$isKindOf(RSString)  # TRUE
#' na_str$value()             # NA
#'
#' @export
RSNAString <- R6::R6Class(
  "RSNAString",
  cloneable = FALSE,
  inherit = RSString,
  public = list(

    #' @description
    #' Initializes an `RSNAString` object that wraps an `NA` character value.
    #' This class is the `NA`-specific counterpart of `RSString`, and should be used
    #' whenever an undefined or missing string is explicitly required.
    #'
    #' Internally, the constructor delegates to `RSString$initialize(NA_character_)`.
    #'
    #' @return An initialized `RSNAString` object (invisible).
    initialize = function() {
      super$initialize(NA_character_)
      invisible(self)
    }
  )
)
RSNAString$isNAClass <- TRUE



# @title Factory Method for RSString
#
# @name stringWith
# 
# @description
# Constructs an `RSString` instance from a character or factor value.
# If the input is `NA`, returns an `RSNAString` instance instead.
#
# @param val A scalar character or factor value.
# @return An instance of `RSString` or `RSNAString`.
#
# @examples
# RSString$stringWith("example")        # RSString
# RSString$stringWith(NA_character_)    # RSNAString
# @export
RSString$stringWith <- function(val) {
  if (is.na(val)) return(RSNAString$new())
  return(RSString$new(val))
}
