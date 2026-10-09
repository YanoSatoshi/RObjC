#' @include primary_class_setup.r
#' @title Declare an RObjC Protocol
#'
#' @description
#' Creates a protocol class generator whose contract is a set of required method
#' names. A protocol can inherit requirements from other protocols through
#' `super_protocols`.
#'
#' @param protocol_name One character string naming the protocol.
#' @param super_protocols A list of RObjC protocol class generators whose
#'   contracts are inherited.
#' @param contract_methods Character vector of required method names declared
#'   directly by this protocol.
#' @param parent_env Parent environment for the generated R6 class.
#' @param version Protocol version. It must not contain `_`.
#'
#' @return The protocol class generator, invisibly.
#'
#' @section Implementation details:
#' The generator inherits from `RSProtocol`. Method names inherited from parent
#' protocols are removed from its own contract, which is stored in
#' `robjc_isa$slot`. Parent protocols and the version are also stored in
#' `robjc_isa`; the metadata is locked after declaration.
#'
#' @examples
#' Printable <- RSProtocolClass("Printable", contract_methods = "print")
#' Printable$classname
#'
#' @export
RSProtocolClass <- function(
  protocol_name,                # character
  super_protocols   = list(),
  contract_methods  = character(0),
  parent_env        = parent.frame(),
  version           = "1.0.0"   # character
) {
  # check contract_methods by char
  stopifnot(
    "`protocol_name` must be a character scalar." =
      is.character(protocol_name) && length(protocol_name) == 1L,
    "`contract_methods` must be character vector." =
      is.character(contract_methods),
    "`super_protocols` must be a list." =
      is.list(super_protocols),
    "Version string must not contain '_'." =
      !grepl("_", version)
  )
  # check super_protocol existances
  flag <- vapply(super_protocols, robjc_isProtocol, logical(1)) 
  if (!all(flag))
    stop(
      sprintf("Not Protocol at indices: %s", paste(which(!flag), sep = ", "))
    )
  # remove methods in contract_methods, those in super_protocols
  inherited_methods <- unique(
    as.character(
      unlist(
        lapply(super_protocols, protocol_getAllContracts),
        use.names = FALSE
      )
    )
  )
  contract_methods <- setdiff(contract_methods, inherited_methods)
  # Instantiate Meta-class
  klass <- do.call(
    R6::R6Class,
    list(
      protocol_name,
      inherit     = RSProtocol,
      parent_env  = parent_env,
      class       = FALSE,
      cloneable   = FALSE,
      lock_class  = TRUE
    )
  )
  # Setup & lock isa
  protocol_make_isa(
    klass,
    super_protocols   = super_protocols,
    contract_methods  = contract_methods,
    version           = version
  )
  robjc_freeze_isa(klass)
  invisible(klass)
}
