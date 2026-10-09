#' @include primary_class_setup.r
#' @title RSSingletonClass
#' @description TODO: document singleton-class construction.
#' @param class_name,super_class,protocols,public,private,active,parent_env,version TODO.
#' @return TODO.
#' @noRd
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
      robjc_isClass(super_generator)
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
