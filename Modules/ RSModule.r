#' RSModule
#'
#' Structural base class for defining singleton-style runtime modules
#' within the RS framework, independently of RSSingleton.
#'
#' This class represents an immutable, singleton-accessed module object
#' used for defining shared, read-only utility functions or configuration objects.
#'
#' Unlike \code{RSSingleton}, this class manages its singleton instance via
#' an explicit constructor function \code{$module()}. Direct instantiation using \code{$new()}
#' is prohibited and will raise an error. Use \code{$module()} to access the shared instance.
#'
#' Subclasses may define public instance properties and methods, but all properties
#' must be defined before calling \code{super$initialize()}, as the module instance will be locked
#' against any further modification or shape changes.
#'
#' @examples
#' # Define a configuration module
#' ConfigModule <- R6::R6Class(
#'   "ConfigModule",
#'   inherit = RSModule,
#'   public = list(
#'     initialize = function() {
#'       self$base_path <- RSString$stringWith("/usr/local/")
#'       self$max_threads <- RSNumber$numberWith(8)
#'       super$initialize()
#'     },
#'
#'     getBasePath = function() self$base_path,
#'     getThreadLimit = function() self$max_threads
#'   )
#' )
#'
#' # Define singleton accessor and instance
#' ConfigModule$instance <- NULL
#' ConfigModule$module <- function() {
#'   if (is.null(ConfigModule$instance)) ConfigModule$instance <- ConfigModule$new()
#'   ConfigModule$instance
#' }
#'
#' # Access the module instance
#' cfg <- ConfigModule$module()
#' print(cfg$getBasePath()$value())       # "/usr/local/"
#' print(cfg$getThreadLimit()$value())    # 8
#'
#' @note
#' Because \code{RSModule} no longer uses private fields for singleton management,
#' subclasses do not need to declare or inherit any private slots. The singleton
#' state is held directly on the class object via \code{$instance}.
#'
#' Developers should avoid using \code{$new()} directly on subclasses of \code{RSModule}.
#' Instead, define a \code{$module()} function and optionally protect the \code{$instance}
#' binding using \code{lockBinding("instance", ClassName)} after first instantiation.

# REMOVE ON RELEASE
if (exists('register_rs_module')) unlockBinding("register_rs_module", environment()) # nolint
warning("Reset 'register_rs_module'")


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
RSModule <- R6::R6Class(
  "RSModule",
  inherit = RSObject,
  cloneable = FALSE,
  public = list(
    # Constructor - must be called once per module
    # All properties must be defined before calling super$initialize().
    initialize = function() {
      super$initialize()
      klass <- self$class()
      if (!is.null(klass$.singleton)) {
        stop(sprintf("Use %s$module() instead of new().", klass$classname))
      }
    },
    # Locks all current bindings in the object to prevent accidental additions or removals.
    lockVariables = function() {
      for (name in ls(self, all.names = TRUE)) lockBinding(name, self)
      lockEnvironment(as.environment(self), bindings = TRUE)
    }
  )
)
register_rs_module(RSModule, RSModule$new())
