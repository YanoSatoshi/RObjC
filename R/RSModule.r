#' @include api_setup.r
#' @title RSModule: Base Class for Singleton Utility Modules
#'
#' @description
#' `RSModule` is a singleton base class for shared utilities. The
#' `RSSingletonClass()` factory creates its instance when the class is declared
#' and stores it in `$instance` and `robjc_isa$slot`.
#'
#' Subclasses can be declared with `RSSingletonClass()` and accessed through
#' `Class$instance`. The current factory does not disable `$new()`, so callers
#' should use the stored instance when singleton identity matters.
#'
#' `initialize()` delegates to [RSObject] and returns the instance. It does not
#' lock instance fields; subclasses can set fields during initialization.
#'
#' @examples
#' \dontrun{
#' ConfigModule <- RSSingletonClass(
#'   "ConfigModule",
#'   super_class = RSModule,
#'   public = list(
#'     base_path = NULL,
#'     initialize = function() {
#'       self$base_path <- RSString$stringWith("/opt/app/")
#'       super$initialize()
#'       invisible(self)
#'     },
#'     getBasePath = function() self$base_path
#'   )
#' )
#' cfg <- ConfigModule$instance
#' cfg$getBasePath()$value()  # "/opt/app/"
#' }
#'
#' @export
RSModule <- RSSingletonClass(
  "RSModule",
  super_class = RSObject,
  version     = "0.0.1 alpha",
  public      = list(
    #' @description Initializes the shared module instance.
    #' @return The initialized instance, invisibly.
    initialize = function() {
      super$initialize()
      invisible(self)
    }
  )
)
