#' @include primary_class_setup.r
#'
#' @title Declare an RObjC Scalar Class
#'
#' @description
#' Creates a subclass of [RSScalar] and sets its class-level `isNAClass` flag.
#' The flag is locked after declaration. For a singleton scalar, the factory
#' sets the flag before creating the first instance, so scalar initialization
#' can read it. In singleton mode, the class's initializer must work without
#' arguments because the factory calls `$new()` without arguments.
#'
#' @param class_name One character string naming the class.
#' @param super_class [RSScalar] or one of its subclass generators.
#' @param is_na_class One logical value. `NA` is allowed; use `TRUE` for a class
#'   whose instances may wrap missing values.
#' @param singleton Whether to create an instance immediately and expose it as
#'   `Class$instance`.
#' @param singleton_accessor Reserved for a future accessor; currently unused.
#'   It can be supplied only when `singleton = TRUE`.
#' @param protocols A list of protocol class generators adopted by the class.
#' @param public,private,active R6 public members, private members, and active
#'   bindings passed to [R6::R6Class()].
#' @param parent_env Parent environment for the generated R6 methods.
#' @param version Class version. It must not contain `_`.
#'
#' @return The scalar class generator, invisibly. Its `isNAClass` binding is
#'   locked; when `singleton = TRUE`, its `instance` binding is also locked.
#'
#' @section Implementation details:
#' Non-singleton classes are built through [RSObjectClass()]. In singleton mode,
#' the factory sets `isNAClass` before creating the instance. The class
#' identity is held in `robjc_isa` on the generator and `.__isa` in its private
#' fields; the instance is also stored in `robjc_isa$slot`.
#'
#' @examples
#' ExampleScalar <- RSScalarClass("ExampleScalar", is_na_class = FALSE,
#'   version = "0.0.2 alpha")
#' ExampleScalar$isNAClass
#'
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
    "Version string must not contain '_'." =
      !grepl("_", version),
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
