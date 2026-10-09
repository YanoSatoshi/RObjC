#' @include primary_class_setup.r
#' @title RSObjectClass
#' @description TODO: document object-class construction.
#' @param class_name,super_class,protocols,public,private,active,parent_env,version TODO.
#' @return TODO.
#' @noRd
#' @export
RSObjectClass <- function(
  class_name,             # character
  super_class = RSObject, # parent class-generator
  protocols   = list(),   # list()
  public      = list(),   # list()
  private     = NULL,     # list()
  active      = NULL,     # list()
  parent_env  = parent.frame(),
  version     = "1.0.0"   # character
) {
  # Get real-super-meta-class
  super_generator <- force(super_class)
  stopifnot(
    "`class_name` must be a character scalar." =
      is.character(class_name) && length(class_name) == 1L,
    "`super_class` must be a kind of RSObject." =
      robjc_isClass(super_generator),
    "Use class declaration api:`RSSingleton` to inherit singleton class." =
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
      active      = active,
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
