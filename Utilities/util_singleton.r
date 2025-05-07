#' @title Register a Singleton Instance for an RSSingleton Subclass
#'
#' @description
#' Assigns a singleton object to an RS-style class by setting the `.singleton` field,
#' and registers an `instance()` method that returns the singleton.
#' This helper also locks the singleton slot to ensure immutability.
#'
#' @param klass The R6 class object to which the singleton belongs.
#' @param obj The singleton instance to register.
#'
#' @return The registered object (invisibly).
#'
#' @examples
#' # register_rs_singleton(MyManager, MyManager$new())
#'
#' @export
register_rs_singleton <- function(klass, obj) {
  klass$.singleton <- obj
  klass$instance <- function() klass$.singleton
  obj$lockVariables()
  lockBinding(".singleton", klass)
  invisible(obj)
}
lockBinding("register_rs_singleton", environment())
