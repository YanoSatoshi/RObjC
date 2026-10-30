#' @include primary_class_setup.r
#' @include runtime_allocation.r
#' @title Declare a Scalar Class and Its NA Companion
#'
#' @description
#' Creates a regular scalar class derived from [RSScalar] and an NA companion
#' class derived from that regular class. By default, the companion class is
#' marked as a singleton and has a shared NA instance.
#'
#' @param class_name A nonempty character scalar. Names beginning with `RS`
#'   produce a companion named `RSNA...`; other names receive an `NA` suffix.
#' @param super_class An [RSScalar] class generator or one of its subclasses.
#' @param na_value A dimensionless length-one missing value, such as `NA_integer_`,
#'   `NA_character_`, `as.Date(NA)`, or `as.POSIXlt(NA)`. The factory passes it directly to the
#'   generated NA class's initializer.
#' @param protocols Additional protocol class generators adopted by the new
#'   classes. Parent declarations are recorded directly on each generated class.
#' @param public,private R6 members for the regular class. If `public`
#'   defines `initialize(val)`, that method must accept the supplied `na_value`
#'   when called on the generated NA subclass and return the result of
#'   `super$initialize(val)` for regular missing values. A default for `val`
#'   is not required.
#' @param parent_env Parent environment for the generated R6 methods.
#' @param version One non-empty, non-missing class version without `_`.
#' @param na_singleton Whether to mark the NA companion class as a singleton.
#'   The default is `TRUE`; [RSFactor] uses `FALSE` so each missing factor can
#'   retain its own levels and ordering.
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
#' A regular class's `$new(NA)` returns its shared NA instance when its
#' initializer propagates the result of `super$initialize(val)`. A class with
#' missing-value metadata can instead construct its NA companion, as [RSFactor]
#' does. Singleton NA companions reject direct `$new()` calls after the shared
#' instance is created; use `Class$na` for them. When `na_singleton = FALSE`,
#' the NA class remains directly constructible. `RSFactor_withFactor()` creates
#' a new [RSNAFactor] for each missing factor to preserve its metadata.
#'
#' @examples
#' Probe <- RSScalarClass("Probe", na_value = NA_integer_,
#'   public = list(initialize = function(val) {
#'     super$initialize(val)
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
  parent_env  = parent.frame(),
  version     = "1.0.0",
  na_singleton = TRUE
) {
  super_generator <- force(super_class)
  if (!is.logical(na_singleton) || length(na_singleton) != 1L || is.na(na_singleton))
    stop("`na_singleton` must be TRUE or FALSE.", call. = FALSE)
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
      !inherits(super_generator, ROBJC_SINGLETON)
  )
  if ((!is.atomic(na_value) && !inherits(na_value, "POSIXlt")) ||
      length(na_value) != 1L ||
      !is.null(dim(na_value)) ||
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
  robjc_install_new(klass)
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
    version   = version,
    na_singleton = na_singleton
  )

  # The normal class slot holds the shared NA instance, matching the NA
  # class's own slot. Its value was initialized from `na_value`.
  klass$robjc_isa$slot <- na_klass$instance
  klass$na <- na_klass$instance
  robjc_freeze_isa(klass)
  # .onLoad() restores these locks for package-defined scalar generators.
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
  version   = "0.1.0 beta",
  na_singleton = TRUE
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
  robjc_install_new(r6generator)
  r6generator$robjc_isa$slot <- r6generator$new(na_value)
  r6generator$instance <- r6generator$robjc_isa$slot
  # .onLoad() restores this lock for package-defined NA companions.
  lockBinding("instance", r6generator)
  if (na_singleton) {
    r6generator$new <- function(...) {
      stop(
        sprintf("%s is a singleton; use %s$na.", na_class_name, klass$classname),
        call. = FALSE
      )
    }
    # .onLoad() restores this lock for package-defined singleton NA companions.
    lockBinding("new", r6generator)
  }
  class(r6generator) <- if (na_singleton)
    c(ROBJC_SINGLETON, ROBJC_CLASS, R6GENERATOR)
  else
    c(ROBJC_CLASS, R6GENERATOR)
  robjc_freeze_isa(r6generator)
  return(r6generator)
}
