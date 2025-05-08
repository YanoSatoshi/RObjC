#' @title Register a Protocol Singleton
#'
#' @description
#' Registers a singleton instance of an `RSProtocol` subclass. It binds the `.singleton` field
#' and defines the class-level accessor `$protocol()`. The registered instance is locked
#' to prevent structural modification.
#'
#' @param klass The R6 class object inheriting from `RSProtocol`.
#' @param obj The instance of the protocol to register.
#'
#' @return The registered singleton instance (invisibly).
#'
#' @examples
#' \dontrun{
#' register_rs_protocol(MyProtocol, MyProtocol$new())
#' }
#'
#' @export
register_rs_protocol <- function(klass, obj) {
  klass$.singleton <- obj
  klass$protocol <- function() klass$.singleton
  obj$lockVariables()
  lockBinding(".singleton", klass)
  invisible(obj)
}
lockBinding("register_rs_protocol", environment())
