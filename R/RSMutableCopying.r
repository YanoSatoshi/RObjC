#' @title RSMutableCopying: Protocol for Mutable Copying
#'
#' @description
#' `RSMutableCopying` is a protocol for objects that can produce a mutable copy.
#' Conforming classes must implement `mutableCopy()`.
#'
#' This is typically used for immutable objects that support conversion into a mutable variant.
#'
#' @section Required Method:
#' - `mutableCopy()` — Returns a mutable copy of the object.
#'
#' @export
RSMutableCopying <- R6::R6Class(
  "RSMutableCopying",
  inherit = RSProtocol,
  cloneable = FALSE,
  public = list(
    #' @description Required method for RSMutableCopying conformance.
    #' @return required method name
    requiredMethods = function() {
      c("mutableCopy")
    }
  )
)