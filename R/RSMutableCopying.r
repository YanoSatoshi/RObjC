#' @include api_setup.r
#' @title RSMutableCopying: Protocol for Mutable Copying
#'
#' @description
#' `RSMutableCopying` is a protocol for objects that can produce a mutable copy.
#' Adoption is a declaration and does not check methods. Because
#' `mutableCopy()` is this protocol's only declared method, it is effectively
#' required when mutable copying is used.
#'
#' This is typically used for immutable objects that support conversion into a mutable variant.
#'
#' @section Declared Method (Required for Mutable Copying):
#' - `mutableCopy()` — Returns a mutable copy of the object.
#'
#' @export
RSMutableCopying <- RSProtocolClass(
  "RSMutableCopying",
  contract_methods = c("mutableCopy"),
  version = "0.0.3 alpha"
)
