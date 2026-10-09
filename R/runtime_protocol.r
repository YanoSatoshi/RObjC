#' @title protocol_make_isa
#' @description TODO.
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


#' @title protocol_getInheritProtocols
#' @description TODO.
#' @noRd
#' @export
protocol_getInheritProtocols <- function(prrotocol) protocol$robjc_isa$protocols


#' @title protocol_getAllInheritProtocols
#' @description TODO.
#' @noRd
#' @export
protocol_getAllInheritProtocols <- function(protocol) {
  bucket <- list()
  for (prtcl in protocol_getInheritProtocols(protocol)) {
    bucket[[length(bucket) + 1L]] <- prtcl
    bucket <- c(bucket, protocol_getAllInheritProtocols(prtcl))
  }
  bucket[!duplicated(bucket)]
}


#' @title protocol_conformsTo
#' @description TODO.
#' @noRd
#' @export
protocol_conformsTo <- function(protocol, target) {
  if (identical(protocol, target)) return(TRUE)
  for (parent in protocol$robjc_isa$protocols)
    if (protocol_conformsTo(parent, target)) return(TRUE)
  return(FALSE)
}


#' @title protocol_getContract
#' @description TODO.
#' @noRd
#' @export
protocol_getContract <- function(protocol) protocol$robjc_isa$slot


#' @title protocol_getAllInheritContracts
#' @description TODO.
#' @noRd
#' @export
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


#' @title protocol_getAllContracts
#' @description TODO.
#' @noRd
#' @export
protocol_getAllContracts <- function(protocol) {
  inherits <- protocol_getAllInheritContracts(protocol)
  domestic <- protocol_getContract(protocol)
  merged <- unique(c(domestic, inherits))
  return(merged)
}
