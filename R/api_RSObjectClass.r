#' @include primary_class_setup.r
#' @title Declare an RObjC Object Class
#'
#' @description
#' Creates an R6 class generator that inherits from an existing RObjC object
#' class. Supply R6 members through `public` and `private`, then
#' create instances with the returned class generator's `$new()` method.
#'
#' A singleton class cannot be used as `super_class`; use
#' [RSSingletonClass()] to declare a singleton subclass.
#'
#' @param class_name One nonempty, nonmissing character string naming the class.
#' @param super_class An RObjC object class generator to inherit from. Defaults
#'   to [RSObject].
#' @param protocols A list of protocol class generators adopted by the class.
#' @param public,private R6 public and private members passed to [R6::R6Class()].
#' @param parent_env Parent environment for the generated R6 methods.
#' @param version One non-empty, non-missing class version without `_`.
#'
#' @return The class generator, invisibly.
#'
#' @section Implementation details:
#' The factory creates an R6 generator with `class = FALSE` and
#' `cloneable = FALSE`. It attaches the class identity as `robjc_isa` on the
#' generator and as `.__isa` in its private fields, then locks the metadata.
#' Instances acquire that identity during [RSObject] initialization.
#'
#' @examples
#' ExampleObject <- RSObjectClass("ExampleObject", version = "0.0.4 alpha")
#' ExampleObject$new()$class()$classname
#'
#' @export
RSObjectClass <- function(
  class_name,             # character
  super_class = RSObject, # parent class-generator
  protocols   = list(),   # list()
  public      = list(),   # list()
  private     = NULL,     # list()
  parent_env  = parent.frame(),
  version     = "1.0.0"   # character
) {
  # Get real-super-meta-class
  super_generator <- force(super_class)
  stopifnot(
    "`class_name` must be one nonempty character string." =
      is.character(class_name) && length(class_name) == 1L &&
      !is.na(class_name) && nzchar(class_name),
    "`super_class` must be a kind of RSObject." =
      robjc_isClass(super_generator),
    "Use `RSSingletonClass()` to declare a singleton subclass." =
      !inherits(super_class, ROBJC_SINGLETON)
  )
  klass <- do.call(
    R6::R6Class,
    list(
      class_name,
      class       = FALSE,
      cloneable   = FALSE,
      inherit     = super_generator,
      public      = public,
      private     = private,
      parent_env  = parent_env
    )
  )
  # Setup & lock isa
  robjc_make_isa(
    klass,
    super_generator,
    protocols = protocols,
    slot      = NULL,
    version   = version
  )
  robjc_freeze_isa(klass)
  invisible(klass)
}
