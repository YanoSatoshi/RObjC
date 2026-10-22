#' @include primary_class_setup.r
#' @title Declare an RObjC Protocol
#'
#' @description
#' Creates a protocol class generator recording declared method names. A
#' protocol can inherit declarations from other protocols through
#' `super_protocols`. Adopting a protocol is a declaration: conformance checks
#' do not verify method implementations. A class must provide the methods
#' needed by the operations it uses. Generators returned by this function
#' cannot create instances with `$new()`.
#'
#' @param protocol_name One nonempty, nonmissing character string naming the protocol.
#' @param super_protocols A list of RObjC protocol class generators whose
#'   contracts are inherited.
#' @param contract_methods Character vector of nonempty, nonmissing method
#'   names declared directly by this protocol. `character(0)` is allowed.
#' @param parent_env Parent environment for the generated R6 class.
#' @param version One non-empty, non-missing protocol version without `_`.
#'
#' @return The protocol class generator, invisibly.
#'
#' @section Implementation details:
#' The generator inherits from `RSProtocol`. Method names inherited from parent
#' protocols are removed from its own contract, which is stored in
#' `robjc_isa$slot`. Parent protocols and the version are also stored in
#' `robjc_isa`; the metadata is locked after declaration. R6 adds a new `$new()`
#' binding to every generated class, so this factory removes and locks it for
#' each protocol generator.
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
  if (!is.list(super_protocols) || !all(vapply(super_protocols, robjc_isProtocol, logical(1))))
    stop("`super_protocols` must be a list of RObjC protocols.", call. = FALSE)
  # check contract_methods by char
  stopifnot(
    "`protocol_name` must be one nonempty character string." =
      is.character(protocol_name) && length(protocol_name) == 1L &&
      !is.na(protocol_name) && nzchar(protocol_name),
    "`contract_methods` must contain nonempty character names." =
      is.character(contract_methods) &&
      all(!is.na(contract_methods) & nzchar(contract_methods))
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
  klass$new <- NULL
  # .onLoad() restores this lock for package-defined protocols after lazy loading.
  lockBinding("new", klass)
  invisible(klass)
}
