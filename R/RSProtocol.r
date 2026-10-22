#' @include robjc-runtime.r
#' @title Root Protocol Generator
#'
#' @description
#' `RSProtocol` is the root generator for RObjC protocols. A protocol records
#' method names that an adopting object class is expected to provide. Declare a
#' new protocol with [RSProtocolClass()], then pass that generator in the
#' `protocols` argument of [RSObjectClass()] or another class factory.
#'
#' Protocols are represented by class generators and cannot be instantiated
#' with `$new()`. An object's [RSObject] `$conformsTo()` method checks
#' the protocol relationships recorded at class declaration time. Declaring
#' conformance does not itself implement or validate the declared methods.
#' The adopting class must provide the methods needed by each operation it uses.
#'
#' @section Implementation details:
#' The root generator has no parent protocol and declares no methods.
#' Its identity metadata is initialized in `primary_class_setup.r`. Protocols
#' made with [RSProtocolClass()] inherit from this generator and record their
#' directly declared contract, parent protocols, and version in class metadata.
#' The root generator's `$new` binding is `NULL` and locked. R6 adds a fresh
#' `$new` binding to each derived generator; [RSProtocolClass()] disables it
#' for declared protocols.
#'
#' @examples
#' Printable <- RSProtocolClass(
#'   "Printable",
#'   contract_methods = "description",
#'   version = "0.0.4 alpha"
#' )
#' robjc_isProtocol(Printable)
#' PrintableObject <- RSObjectClass(
#'   "PrintableObject",
#'   protocols = list(Printable),
#'   version = "0.0.4 alpha"
#' )
#' PrintableObject$new()$conformsTo(Printable)
#'
#' @seealso [RSProtocolClass()], [RSObjectClass()], [robjc_isProtocol()], [RSObject]
#' @export
RSProtocol <- R6::R6Class(
  "RSProtocol",
  class     = FALSE,
  cloneable = FALSE,
  portable  = TRUE,
  active    = NULL,
  inherit   = NULL,
  private   = list(
    .__isa = NULL
  )
)

RSProtocol$new <- NULL
lockBinding("new", RSProtocol)
