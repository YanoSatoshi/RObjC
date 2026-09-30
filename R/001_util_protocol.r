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
#'
#' @param klass The R6 class object inheriting from `RSProtocol`.
#' @param obj The protocol instance to register.
#'
#' @return The registered singleton instance (invisibly).
#'
#' @examples
#' \dontrun{
#' register_rs_protocol(MyProtocol, MyProtocol$new())
#' MyProtocol$protocol()  # Retrieve the registered protocol instance
#' }
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
#' Registers a protocol conformance relationship between a protocol instance and a class object
#' within the robjc object system. This function updates the class’s internal protocol table
#' (`robjc$protocols`) so that instances of the class will return `TRUE` when `conformsTo()` is called.
#'
#' Unlike inheritance via `inherit`, this function allows existing classes to retroactively declare
#' conformance to a given protocol, without modifying their inheritance chain.
#' 
#' If klass$robjc_isa has not yet been initialized, the function instantiates klass using args
#' to initialize its robjc class metadata before registering the protocol.
#'
#' @param protocol An object that is an instance of a subclass of `RSProtocol`.
#' @param klass An RSObject.
#' @param init_args Named list with arguments to call `klass$new(...)`.
#' 
#'
#' @return None. The class is modified in-place by appending the protocol to `klass$robjc_isa$protocols`.
#'
#' @examples
#' \dontrun{
#' conform_protocol_to_class(MyEncodingProtocol$protocol(), MyEncoderClass)
#' obj <- MyEncoderClass$new()
#' obj$conformsTo("MyEncodingProtocol")  # TRUE
#' 
#' conform_protocol_to_class(
#'   MyNameProtocol$protocol(),
#'   MyNameClass,
#'   init_args = list(name = "hoge")
#' )
#' obj <- MyNameClass$new("foo")
#' obj$conformsTo("MyNameProtocol") # TRUE
#' }
#'
#' @seealso
#' \code{\link[=RSProtocol]{RSProtocol}} – Protocol base class.
#' \code{\link[=RSObject]{RSObject}} – See method \code{conformsTo()}.
#'
#' @note
#' This function assumes that `klass$robjc_isa$inherit_chain` and `klass$robjc_isa$protocols`
#' have already been initialized. These are typically set during class construction
#' via `RSObject$initialize()`.
#'
#' @export
conform_protocol_to_class <- function(protocol, klass, init_args = list()) {
  stopifnot(protocol$isKindOf(RSProtocol))
  if (is.null(klass$robjc_isa)) do.call(klass$new, init_args)
  prtcl_cls <- if (is_robjc_instance(protocol)) protocol$class() else protocol
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
#' It returns `FALSE` if the object is not an `RSObject`, or does not implement the `conformsTo()` method.
#'
#' @param obj Any R object, typically an `RSObject` instance.
#' @param protocol A protocol object, such as one implementing `RSProtocol`.
#'
#' @return `TRUE` if the object conforms to the protocol, `FALSE` otherwise.
#'
util_confirm_protocol <- function(obj, protocol) {
  stopifnot("Obj is not robjc instance." = is_robjc_instance(obj))
  obj$conformsTo(protocol)
}