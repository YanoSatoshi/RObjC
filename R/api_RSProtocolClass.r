#' @include primary_class_setup.r
#' @title RSProtocolClass
#' @description TODO: document protocol-class construction.
#' @param protocol_name,super_protocols,contract_methods,parent_env,version TODO.
#' @return TODO.
#' @noRd
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
      is.list(super_protocols)
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
