#' @include robjc-runtime.r
#' @title Root Protocol Generator
#'
#' @description
#' `RSProtocol` is the root generator for RObjC protocols. A protocol records
#' method names that an adopting object class is expected to provide. Declare a
#' new protocol with [RSProtocolClass()], then pass that generator in the
#' `protocols` argument of [RSObjectClass()] or another class factory.
#'
#' Protocols are represented by class generators, rather than by instances
#' created with `$new()`. An object's [RSObject] `$conformsTo()` method checks
#' the protocol relationships recorded at class declaration time. Declaring
#' conformance does not itself implement or validate the required methods.
#'
#' @section Implementation details:
#' The root generator has no parent protocol and declares no required methods.
#' Its identity metadata is initialized in `primary_class_setup.r`. Protocols
#' made with [RSProtocolClass()] inherit from this generator and record their
#' directly declared contract, parent protocols, and version in class metadata.
#' The root's `initialize()` assigns R class attributes to an instance; it is
#' part of the runtime and should not be overridden by protocol declarations.
#'
#' @examples
#' Printable <- RSProtocolClass(
#'   "Printable",
#'   contract_methods = "description",
#'   version = "0.0.3 alpha"
#' )
#' robjc_isProtocol(Printable)
#' PrintableObject <- RSObjectClass(
#'   "PrintableObject",
#'   protocols = list(Printable),
#'   version = "0.0.3 alpha"
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
  public    = list(
    #' @description Assigns R class attributes when a protocol instance is
    #'   created. Protocol declarations normally use generators directly.
    #' @return The initialized instance, invisibly.
    initialize = function() {
      # DO NOT OVERRIDE
      class(self) <- vapply(
        robjc_inheritChain(private$.__isa),
        function(klass) klass$classname,
        character(1)
      )
      invisible(self) 
    }
  ),
  private   = list(
    .__isa = NULL
  )
)
