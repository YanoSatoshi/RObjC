#' @title Register a Singleton Instance of an RSModule Subclass
#'
#' @description
#' Registers a singleton module object on the given class and defines its `$module()` accessor.
#' This function also locks the `.singleton` slot and the module object itself using `lockVariables()`.
#' It is invoked internally when registering an instance of an `RSModule` subclass.
#'
#' @param klass The R6 class object (subclass of `RSModule`)
#' @param obj The instance of the module to register
#'
#' @return The registered singleton object (invisibly).
#'
#' @examples
#' register_rs_module(ConfigModule, ConfigModule$new())
#'
#' @export
register_rs_module <- function(klass, obj) {
  klass$.singleton <- obj
  if (!identical(klass, RSModule)) {
    klass$module <- function() klass$.singleton
  } else {
    RSModule$module <- function() {
      stop("RSModule is an abstract base class. Use a subclass's $module() instead.")
    }
  }
  obj$lockVariables()
  lockBinding(".singleton", klass)
  invisible(obj)
}
lockBinding("register_rs_module", environment())
