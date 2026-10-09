# TODO: REMOVE ON RELEASE
tmp <- "register_rs_protocol"
if (Sys.getenv("DEBUG_MODE") == "TRUE" && exists(tmp, environment(), inherits = FALSE)) {
  if (bindingIsLocked(tmp, environment())) {
    unlockBinding(tmp, environment())
    cat(sprintf("Unlocks binding %s.\n", tmp))
  } else {
    cat(sprintf("%s binding is locked.\n", tmp))
  }
}


#' @include RSProtocol.r
#' @title Register a Protocol Singleton
#'
#' @description
#' Registers a singleton instance of an `RSProtocol` subclass and stores it in the
#' class's internal `robjc_isa$slot` field. An accessor method `$protocol()` is automatically
#' defined on the class for retrieving the registered instance.
#'
#' Unlike `RSModule` or `RSSingleton`, protocol instances are intended to be declarative
#' and immutable. As such, this function calls `lockVariables()` on the instance after
#' registration to prevent any structural modification.
#'
#' If the class already has a `$protocol()` accessor, an error is raised to avoid redefinition.
#' The `slot` binding is also locked using `lockBinding()` to ensure singleton integrity.
#' This is a legacy registration helper. New protocol declarations use
#' `RSProtocolClass()` and store their contracts directly in class metadata.
#'
#' @param klass The R6 class object inheriting from `RSProtocol`.
#' @param obj The protocol instance to register.
#'
#' @return The registered singleton instance (invisibly).
#'
#' @seealso
#' \code{\link{RSProtocol}} – The base class for protocol declarations in the robjc system.
#'
#' @export
register_rs_protocol <- function(klass, obj) {
  stopifnot(obj$isKindOf(RSProtocol))
  klass$robjc_isa$slot <- obj
  if (is.null(klass$protocol)) {
    klass$protocol <- function() klass$robjc_isa$slot
  } else {
    stop(sprintf("%s has already registered by register_rs_protocol."))
  }
  obj$lockVariables()
  lockBinding("slot", klass$robjc_isa)
  invisible(obj)
}
lockBinding("register_rs_protocol", environment())





#' @title Declare Protocol Conformance on a Class Object
#'
#' @description
#' Legacy helper that appends a protocol to a class's `robjc_isa$protocols`.
#' Classes declared with `RSObjectClass()` have locked class metadata, so their
#' protocols must be supplied through the factory's `protocols` argument instead.
#'
#' The helper can only modify a class whose protocol binding remains unlocked.
#' If its metadata is absent, it calls `klass$new()` with `init_args` first.
#'
#' @param protocol An object that is an instance of a subclass of `RSProtocol`.
#' @param klass An RSObject.
#' @param init_args Named list with arguments to call `klass$new(...)`.
#' 
#'
#' @return `NULL` if the protocol is already listed; otherwise the updated
#'   protocol list.
#'
#' @seealso
#' \code{\link[=RSProtocol]{RSProtocol}} – Protocol base class.
#' \code{\link[=RSObject]{RSObject}} – See method \code{conformsTo()}.
#'
#' @note
#' The `protocols` binding is locked by the current class factories. Use the
#' factory argument for new declarations.
#'
#' @export
conform_protocol_to_class <- function(protocol, klass, init_args = list()) {
  stopifnot(protocol$isKindOf(RSProtocol))
  if (is.null(klass$robjc_isa)) do.call(klass$new, init_args)
  prtcl_cls <- if (robjc_isRobjcObject(protocol)) protocol$class() else protocol
  contains <- any(
    vapply(
      klass$robjc_isa$protocols,
      function(proto) identical(proto, prtcl_cls),
      logical(1)
    )
  )
  if (contains) return(NULL)
  prtcl <- list(prtcl_cls)
  names(prtcl) <- prtcl_cls$classname
  klass$robjc_isa$protocols <- c(klass$robjc_isa$protocols, prtcl)
}





#' @title Confirm protocol conformance of an object
#'
#' @description
#' A utility function to confirm whether a given object conforms to a specified protocol.
#' It requires an RObjC instance and delegates to its `conformsTo()` method.
#'
#' @param obj An RObjC instance.
#' @param protocol A protocol object, such as one implementing `RSProtocol`.
#'
#' @return `TRUE` if the object conforms to the protocol, `FALSE` otherwise.
#'
util_confirm_protocol <- function(obj, protocol) {
  stopifnot("Obj is not robjc instance." = robjc_isRobjcObject(obj))
  obj$conformsTo(protocol)
}
