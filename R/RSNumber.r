#' @include RSScalar.r
#' @title RSNumber: Numeric Scalar Wrapper
#'
#' @description
#' `RSNumber` wraps a numeric scalar (double or integer) in a value-object style,
#' inheriting common scalar behavior from `RSScalar`.
#' It disallows `NA` values directly; use `RSNANumber` instead.
#'
#' @section Usage:
#' Use \code{RSNumber_withValue(val)} to safely construct from a numeric scalar.
#' If \code{val} is \code{NA}, it returns \code{RSNANumber}.
#' 
#' @section See Also:
#' [RSNANumber] for handling NA values.
#'
#' @export
RSNumber <- RSScalarClass(
  "RSNumber",
  version = "0.0.2 alpha",
  super_class = RSScalar,
  is_na_class = FALSE,
  public = list(

    #' @description
    #' Initializes an `RSNumber` instance by wrapping a scalar numeric value.
    #' The input must be of type numeric (either `double` or `integer`) and must not be `NA`.
    #' If the input is `NA`, use the `RSNANumber` class instead.
    #'
    #' This method performs strict type validation and delegates scalar storage
    #' and behavior to the superclass `RSScalar`.
    #'
    #' @param val A scalar numeric value. Must not be `NA`.
    #'
    #' @return An initialized `RSNumber` object (invisible).
    initialize = function(val) {
      if (!is.numeric(val) || is.na(val)) {
        stop("val must be of class numeric and not NA")
      }
      super$initialize(val)
      invisible(self)
    }
  )
)




#' @title RSNANumber: Wrapper for NA-numeric Scalar
#'
#' @description
#' `RSNANumber` is a subclass of `RSNumber` used specifically to represent `as.numeric(NA)` values
#' in a safe, class-distinct manner. Instances of this class are recognized by `isNAClass == TRUE`.
#'
#' @note
#' This class should not be instantiated directly under most circumstances.
#' Use `RSNumber_withValue(val)` to safely wrap numeric values.
#'
#' @export
RSNANumber <- RSScalarClass(
  "RSNANumber",
  version = "0.0.2 alpha",
  super_class = RSNumber,
  is_na_class = TRUE,
  public = list(

    #' @description
    #' Initializes an `RSNANumber` instance that represents a scalar numeric `NA` value.
    #' This class exists specifically to distinguish between regular numeric values and `NA`,
    #' and should be used when an `NA` must be explicitly wrapped in the scalar object model.
    #'
    #' Internally, this method delegates to the parent `RSNumber` class,
    #' passing `as.numeric(NA)` as the scalar value.
    #'
    #' @return An initialized `RSNANumber` object (invisible).
    initialize = function() {
      super$initialize(0L)
      private$.val <- as.numeric(NA)
      invisible(self)
    }
  )
)
