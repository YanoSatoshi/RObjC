#' @include RSProtocol.r
#' @title RSCopying: Protocol for Object Copying
#'
#' @description
#' `RSCopying` is a protocol that defines a contract for objects that can produce a
#' copy of themselves. Conforming objects must implement the method `copy()`.
#'
#' This protocol typically applies to immutable objects or value types that support
#' shallow or type-converting copy semantics.
#'
#' @section Required Method:
#' - `copy()` — Returns a copy of the object. May return an immutable copy if the object is mutable.
#'
#' @examples
#' \dontrun{
#' MyClass <- RSObjectClass("MyClass", super_class = RSObject,
#'   protocols = list(RSCopying), public = list(
#'   value = NULL,
#'   initialize = function(val) { super$initialize(); self$value <- val },
#'   copy = function() {
#'     MyClass$new(self$value)
#'   }
#' ))
#' }
#'
#' @export
RSCopying <- R6::R6Class(
  "RSCopying",
  class     = FALSE,
  cloneable = FALSE,
  portable  = TRUE,
  active    = NULL,
  inherit   = RSProtocol
)
