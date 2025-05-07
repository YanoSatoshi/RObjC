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



#' @title Factory Method for RSString
#'
#' @description
#' Constructs an `RSString` instance from a character or factor value.
#' If the input is `NA`, returns an `RSNAString` instance instead.
#'
#' @param val A scalar character or factor value.
#' @return An instance of `RSString` or `RSNAString`.
#'
#' @examples
#' RSString$stringWith("example")        # RSString
#' RSString$stringWith(NA_character_)    # RSNAString
#'
#' @export
RSString$stringWith <- function(val) {
  if (is.na(val)) return(RSNAString$new())
  return(RSString$new(val))
}



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
    initialize = function() {
      super$initialize(NA_character_)
      invisible(self)
    }
  )
)
RSNAString$isNAClass <- TRUE
