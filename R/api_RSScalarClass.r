#' Declare a Scalar Class and Its NA Companion
#'
#' @description
#' Creates a regular scalar class derived from [RSScalar] and an NA companion
#' class derived from that regular class. The companion is created as a
#' singleton while this factory runs.
#'
#' @param class_name A nonempty character scalar. Names beginning with `RS`
#'   produce a companion named `RSNA...`; other names receive an `NA` suffix.
#' @param super_class An [RSScalar] class generator or one of its subclasses.
#' @param na_value A length-one missing value, such as `NA_integer_`,
#'   `NA_character_`, or `as.Date(NA)`. The factory passes it directly to the
#'   generated NA class's initializer.
#' @param protocols Protocol class generators adopted by the new classes.
#' @param public,private,active R6 members for the regular class. If `public`
#'   defines `initialize(val)`, that method must accept the supplied `na_value`
#'   when called on the generated NA subclass and pass it to
#'   `super$initialize(val)`. A default for `val` is not required.
#' @param parent_env Parent environment for the generated R6 methods.
#' @param version Class version; `_` is reserved for class identifiers.
#'
#' @return The regular class generator, invisibly. Its `na` binding and its
#'   `robjc_isa$slot` both reference the shared NA instance. The NA class
#'   generator is available through `Class$na$class()`. Class identity and
#'   shared-instance bindings are locked.
#'
#' @details
#' The factory builds the regular class and attaches its identity, then builds
#' its NA subclass. It sets `isNAClass` before calling the NA subclass's
#' `$new(na_value)`. The inherited initializer stores the missing value through
#' [RSScalar] when a subclass calls `super$initialize(val)`. The shared instance
#' is saved in both classes' `robjc_isa$slot` and exposed as `Class$na`.
#' The generated NA class still has a callable `$new()` method; callers should
#' use `Class$na` when they need the shared instance.
#'
#' @examples
#' Probe <- RSScalarClass("Probe", na_value = NA_integer_,
#'   public = list(initialize = function(val) {
#'     super$initialize(val)
#'     invisible(self)
#'   }))
#' Probe$na$isKindOf(Probe)
#'
#' @export
RSScalarClass <- function(
  class_name,
  super_class = RSScalar,
  na_value    = NA_integer_,
  protocols   = list(),
  public      = list(),
  private     = NULL,
  active      = NULL,
  parent_env  = parent.frame(),
  version     = "1.0.0"
) {
  super_generator <- force(super_class)
  stopifnot(
    "`class_name` must be one nonempty character string." =
      is.character(class_name) && length(class_name) == 1L &&
      !is.na(class_name) && nzchar(class_name),
    "`super_class` must inherit from RSScalar." =
      robjc_isClass(super_generator) &&
      any(vapply(
        robjc_inheritChain(super_generator$robjc_isa),
        function(klass) identical(klass, RSScalar), logical(1)
      )),
    "Use `RSSingletonClass()` to declare a singleton subclass." =
      !inherits(super_generator, ROBJC_SINGLETON),
    "Version string must not contain '_'." =
      is.character(version) && length(version) == 1L &&
      !is.na(version) && !grepl("_", version)
  )
  if (!is.atomic(na_value) || length(na_value) != 1L ||
      !isTRUE(is.na(na_value)) || isTRUE(is.nan(na_value))) {
    stop("`na_value` must be a scalar NA.", call. = FALSE)
  }

  klass <- do.call(
    R6::R6Class,
    list(
      classname   = class_name,
      inherit     = super_generator,
      class       = FALSE,
      cloneable   = FALSE,
      public      = public,
      private     = private,
      active      = active,
      parent_env  = parent_env
    )
  )
  klass$isNAClass <- FALSE
  robjc_make_isa(
    klass,
    super_generator,
    protocols = protocols,
    version = version
  )
  na_class_name <- if (startsWith(class_name, "RS"))
    sub("^RS", "RSNA", class_name)
  else
    paste0(class_name, "NA")
  na_klass <- rsscalar_make_na_class(
    na_class_name,
    klass,
    na_value,
    parent_env,
    protocols = protocols,
    version   = version
  )

  # The normal class slot holds the shared NA instance, matching the NA
  # class's own slot. Its value was initialized from `na_value`.
  klass$robjc_isa$slot <- na_klass$instance
  klass$na <- na_klass$instance
  robjc_freeze_isa(klass)
  lockBinding("isNAClass", na_klass)
  lockBinding("isNAClass", klass)
  lockBinding("na", klass)
  invisible(klass)
}



rsscalar_make_na_class <- function(
  na_class_name,
  klass,
  na_value,
  parent_env,
  protocols = list(),
  version   = "0.0.3 alpha"
) {
  r6generator <- do.call(
    R6::R6Class,
    list(
      classname   = na_class_name,
      inherit     = klass,
      class       = FALSE,
      cloneable   = FALSE,
      parent_env  = parent_env
    )
  )
  r6generator$isNAClass <- TRUE
  robjc_make_isa(
    r6generator,
    klass,
    protocols = protocols,
    slot      = NULL,
    version   = version
  )
  r6generator$robjc_isa$slot <- r6generator$new(na_value)
  r6generator$instance <- r6generator$robjc_isa$slot
  lockBinding("instance", r6generator)
  class(r6generator) <- c(ROBJC_SINGLETON, ROBJC_CLASS, R6GENERATOR)
  robjc_freeze_isa(r6generator)
  return(r6generator)
}
