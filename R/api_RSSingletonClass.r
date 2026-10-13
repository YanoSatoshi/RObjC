#' @include primary_class_setup.r
#' @title Declare an RObjC Singleton Class
#'
#' @description
#' Creates an R6 class generator and immediately creates its instance. Access
#' the instance through `Class$instance`.
#'
#' The factory currently leaves `$new()` callable. Use `$instance` when callers
#' must share the object created at declaration time.
#' The class's initializer must work without arguments because the factory
#' calls `$new()` without arguments.
#'
#' @param class_name One character string naming the class.
#' @param super_class An RObjC object class generator to inherit from. Defaults
#'   to [RSObject].
#' @param protocols A list of protocol class generators adopted by the class.
#' @param public,private,active R6 public members, private members, and active
#'   bindings passed to [R6::R6Class()].
#' @param parent_env Parent environment for the generated R6 methods.
#' @param version Class version. It must not contain `_`.
#'
#' @return The singleton class generator, invisibly.
#'
#' @section Implementation details:
#' The generator uses `class = FALSE` and `cloneable = FALSE`. The factory
#' stores the instance in both `Class$instance` and `Class$robjc_isa$slot`,
#' then locks the instance binding and class metadata.
#'
#' @examples
#' ExampleSingleton <- RSSingletonClass("ExampleSingleton",
#'   version = "0.0.3 alpha")
#' ExampleSingleton$instance$class()$classname
#'
#' @export
RSSingletonClass <- function(
  class_name,              # character
  super_class = RSObject,  # parent class-generator
  protocols   = list(),    # list()
  public      = list(),    # list()
  private     = NULL,      # list()
  active      = NULL,      # list()
  parent_env  = parent.frame(),
  version     = "1.0.0"    # character
) {
  super_generator <- force(super_class)
  stopifnot(
    "`class_name` must be a character scalar." =
      is.character(class_name) && length(class_name) == 1L,
    "`super_class` must be a kind of RSObject." =
      robjc_isClass(super_generator),
    "Version string must not contain '_'." =
      !grepl("_", version)
  )
  klass <- do.call(
    R6::R6Class,
    list(
      class_name,
      inherit     = super_generator,
      class       = FALSE,
      cloneable   = FALSE,
      public      = public,
      private     = private,
      active      = active,
      parent_env  = parent_env
    )
  )
  # Setup isa
  robjc_make_singleton_isa(
    klass,
    super_generator,
    protocols = protocols,
    version   = version
  )
  robjc_freeze_isa(klass)
  lockBinding("instance", klass)
  invisible(klass)
}
