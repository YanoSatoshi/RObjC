#' @include api_setup.r
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
RSMutableCopying <- RSProtocolClass(
  "RSMutableCopying",
  contract_methods = c("mutableCopy"),
  version = "0.0.2 alpha"
)