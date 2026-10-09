#' @include runtime_robjc.r

#' @title Initialize Protocol Class Metadata
#' @description Internal helper that attaches protocol identity, parent
#'   protocols, contract methods, and a version to an R6 generator.
#' @param r6generator An R6 class generator for a protocol.
#' @param super_protocols Parent protocol generators.
#' @param contract_methods Character vector of directly declared method names.
#' @param version Protocol version.
#' @return `r6generator`, invisibly.
#' @noRd
protocol_make_isa <- function(
  r6generator,
  super_protocols   = list(),
  contract_methods  = character(0),
  version           = "1.0.0"
) {
  stopifnot(
    "`contract_methods` must be a character vector." =
      is.character(contract_methods)
  )
  for (protocol in super_protocols) {
    if (!inherits(protocol$robjc_isa, ISA_PROTOCOL))
      stop(
        sprintf("In `super_protocols`, %s is not a protocol.", protocol$classname),
        call. = FALSE
      )
  }
  robjc_make_isa(
    r6generator,
    super_class = RSProtocol,
    protocols   = super_protocols,
    slot        = contract_methods,
    version     = version
  )
  class(r6generator) <- ROBJC_CLASS
  invisible(r6generator)
}


#' @title Get a Protocol's Direct Parents
#' @description Returns the protocols named as direct parents when a protocol
#'   was declared.
#' @param protocol A protocol class generator.
#' @return A list of direct parent protocol generators.
#' @keywords internal
protocol_getInheritProtocols <- function(protocol) protocol$robjc_isa$protocols


#' @title Get All Ancestor Protocols
#' @description Recursively collects a protocol's direct and indirect parent
#'   protocols, removing duplicates.
#' @param protocol A protocol class generator.
#' @return A list of ancestor protocol generators.
#' @keywords internal
protocol_getAllInheritProtocols <- function(protocol) {
  bucket <- list()
  for (prtcl in protocol_getInheritProtocols(protocol)) {
    bucket[[length(bucket) + 1L]] <- prtcl
    bucket <- c(bucket, protocol_getAllInheritProtocols(prtcl))
  }
  bucket[!duplicated(bucket)]
}


#' @title Check Protocol Inheritance
#' @description Tests whether a protocol is identical to another protocol or
#'   inherits from it, directly or indirectly.
#' @param protocol A protocol class generator to test.
#' @param target The protocol class generator to look for.
#' @return `TRUE` if `protocol` conforms to `target`, otherwise `FALSE`.
#' @keywords internal
protocol_conformsTo <- function(protocol, target) {
  if (identical(protocol, target)) return(TRUE)
  for (parent in protocol$robjc_isa$protocols)
    if (protocol_conformsTo(parent, target)) return(TRUE)
  return(FALSE)
}


#' @title Get a Protocol's Direct Contract
#' @description Returns method names declared directly by a protocol, excluding
#'   requirements inherited from parent protocols.
#' @param protocol A protocol class generator.
#' @return A character vector of method names.
#' @keywords internal
protocol_getContract <- function(protocol) protocol$robjc_isa$slot


#' @title Get Inherited Protocol Requirements
#' @description Collects unique method names required by all ancestor protocols.
#' @param protocol A protocol class generator.
#' @return A character vector of inherited method names.
#' @keywords internal
protocol_getAllInheritContracts <- function(protocol) {
  inherit_protocols <- protocol_getAllInheritProtocols(protocol)
  unique(
    as.character(
      unlist(
        lapply(inherit_protocols, protocol_getContract),
        use.names = FALSE
      )
    )
  )
}


#' @title Get All Protocol Requirements
#' @description Combines a protocol's directly declared method names with all
#'   requirements inherited from its parent protocols.
#' @param protocol A protocol class generator.
#' @return A character vector of unique method names.
#' @examples
#' Printable <- RSProtocolClass("Printable", contract_methods = "print")
#' robjc:::protocol_getAllContracts(Printable)
#' @keywords internal
protocol_getAllContracts <- function(protocol) {
  inherits <- protocol_getAllInheritContracts(protocol)
  domestic <- protocol_getContract(protocol)
  merged <- unique(c(domestic, inherits))
  return(merged)
}
