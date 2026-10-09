#' @include RSScalar.r
#' @title RSString: Character or Factor Scalar Wrapper
#'
#' @description
#' `RSString` is a scalar wrapper class that encapsulates character or factor values.
#' It inherits from `RSScalar` and provides support for equality comparison,
#' hashing, and sorting. This class allows factor levels to be treated interchangeably
#' with character strings at the scalar abstraction level.
#'
#' The constructor accepts a single-length character or factor value.
#' Use the factory method `RSString_withValue()` to construct an instance safely,
#' especially when the input may be `NA`.
#'
#' @section See Also:
#' [RSNAString] for representing `NA_character_` values in a class-based form.
#'
#' @section Usage:
#' Use \code{RSString_withValue(val)} to safely construct from a character or factor.
#' If \code{val} is \code{NA}, it returns \code{RSNAString}.
#' 
#' @examples
#' s <- RSString_withValue("hello")
#' s$value()  # "hello"
#'
#' @export
RSString <- RSScalarClass(
  "RSString",
  version = "0.0.2 alpha",
  super_class = RSScalar,
  is_na_class = FALSE,
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




#' @title RSNAString: Wrapper for NA-character Scalar
#'
#' @description
#' `RSNAString` is a subclass of `RSString` used specifically to represent `NA_character_`
#' values. This class conforms to the `RSScalar` interface while clearly indicating
#' that it wraps a missing string value.
#'
#' This class is typically instantiated internally via `RSString_withValue()`.
#' The `isNAClass` flag is set to TRUE to enable differentiated behavior in comparison logic.
#'
#' @examples
#' na_str <- RSString_withValue(NA_character_)
#' na_str$isKindOf(RSString)  # TRUE
#' na_str$value()             # NA
#'
#' @export
RSNAString <- RSScalarClass(
  "RSNAString",
  version = "0.0.2 alpha",
  super_class = RSString,
  is_na_class = TRUE,
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
