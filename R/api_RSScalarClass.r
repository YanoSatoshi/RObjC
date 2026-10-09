#' @include primary_class_setup.r
#'
#' @title Declare a Scalar Class
#'
#' @description
#' Set the scalar class flag before its first instance is created. This is
#' necessary for singleton scalars, whose instance is created by the factory.
#'
#' @param class_name Name of the class.
#' @param super_class An RSScalar class generator or one of its subclasses.
#' @param is_na_class Logical scalar. `NA` is allowed for classes such as RSNull.
#' @param singleton Whether to create a singleton instance immediately.
#' @param singleton_accessor Reserved for a future accessor; currently unused.
#' @param protocols Protocols adopted by the class.
#' @param public,private,active R6 class members.
#' @param parent_env Parent environment for R6 methods.
#' @param version Class version.
#' @return The class generator.
#' @export
RSScalarClass <- function(
  class_name,
  super_class = RSScalar,
  is_na_class,
  singleton   = FALSE,
  singleton_accessor = NULL,
  protocols   = list(),
  public      = list(),
  private     = NULL,
  active      = NULL,
  parent_env  = parent.frame(),
  version     = "1.0.0"
) {
  stopifnot(
    "`is_na_class` must be one logical value." =
      is.logical(is_na_class) && length(is_na_class) == 1L,
    "`singleton` must be TRUE or FALSE." =
      is.logical(singleton) && length(singleton) == 1L && !is.na(singleton),
    "`singleton_accessor` requires `singleton = TRUE`." =
      singleton || is.null(singleton_accessor),
    "`super_class` must inherit from RSScalar." =
      robjc_isClass(super_class) &&
      any(vapply(
        robjc_inheritChain(super_class$robjc_isa),
        function(klass) identical(klass, RSScalar),
        logical(1)
      ))
  )

  set_scalar_flag <- function(klass) {
    klass$isNAClass <- is_na_class
    lockBinding("isNAClass", klass)
    invisible(klass)
  }

  if (singleton) {
    super_generator <- force(super_class)
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
    set_scalar_flag(klass)
    # Setup isa
    robjc_make_singleton_isa(
      klass,
      super_generator,
      protocols = protocols,
      version   = version
    )
    robjc_freeze_isa(klass)
    lockBinding("instance", klass)
  } else {
    klass <- RSObjectClass(
      class_name,
      super_class = super_class,
      protocols = protocols,
      public = public,
      private = private,
      active = active,
      parent_env = parent_env,
      version = version
    )
    set_scalar_flag(klass)
  }
  invisible(klass)
}
