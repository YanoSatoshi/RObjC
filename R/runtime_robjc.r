# ------------
# Internal S3 markers for identity records.
# ------------
ISA_ROOT      <- "RSIsA"

ISA_OBJECT    <- "RSObjectIsA"

ISA_PROTOCOL  <- "RSProtocolIsA"

lockBinding("ISA_ROOT",     environment())
lockBinding("ISA_OBJECT",   environment())
lockBinding("ISA_PROTOCOL", environment())



# ------------
#' Internal RObjC Type Markers
#'
#' @description Character constants used to mark R6 generators and RObjC
#'   identity records. These are implementation details of the runtime.
#' @format Each marker is one character string.
#' @aliases ROBJC_CLASS ROBJC_SINGLETON ISA_ROOT ISA_OBJECT ISA_PROTOCOL
#' @keywords internal
#' @export
R6GENERATOR     <- "R6ClassGenerator"
ROBJC_CLASS     <- "RobjcClass"
ROBJC_SINGLETON <- "RobjcSingleton"
lockBinding("R6GENERATOR",      environment())
lockBinding("ROBJC_CLASS",      environment())
lockBinding("ROBJC_SINGLETON",  environment())



# ------------
# MetaClass
# ------------
#' @title Check for an RObjC Class Generator
#'
#' @description
#' Tests whether an object is a class or protocol generator created by RObjC.
#' This includes both object classes and protocol classes.
#'
#' @param obj Any R object.
#'
#' @return `TRUE` for an RObjC generator, `FALSE` otherwise.
#'
#' @section Implementation details:
#' The check requires an environment with the `RobjcClass` marker and an
#' `robjc_isa` environment with the `RSIsA` marker.
#'
#' @examples
#' robjc_isMetaClass(RSObject)
#' robjc_isMetaClass(R6::R6Class("NotRobjc"))
#'
#' @export
robjc_isMetaClass <- function(obj) {
  is.environment(obj) &&
    inherits(obj, ROBJC_CLASS) &&
    !is.null(obj$robjc_isa) &&
    inherits(obj$robjc_isa, ISA_ROOT)
}



# ------------
# MetaClass ClassObj
# ------------
#' @title Check for an RObjC Object Class
#' @description Tests whether an object is an RObjC object class generator,
#'   including singleton and scalar classes. Protocol generators return `FALSE`.
#' @param obj Any R object.
#' @return `TRUE` for an object class generator, `FALSE` otherwise.
#' @examples
#' robjc_isClass(RSObject)
#' robjc_isClass(RSProtocol)
#' @export
robjc_isClass <- function(obj) {
  robjc_isMetaClass(obj) &&
    inherits(obj$robjc_isa, ISA_OBJECT)
}



# ------------
# MetaClass ProtocolObj
# ------------
#' @title Check for an RObjC Protocol Class
#' @description Tests whether an object is an RObjC protocol class generator.
#' @param obj Any R object.
#' @return `TRUE` for a protocol generator, `FALSE` otherwise.
#' @examples
#' Printable <- RSProtocolClass("Printable", contract_methods = "print")
#' robjc_isProtocol(Printable)
#' @export
robjc_isProtocol <- function(obj) {
  robjc_isMetaClass(obj) &&
    inherits(obj$robjc_isa, ISA_PROTOCOL)
}



# ------------
# Instance
# ------------
#' @title Check for an RObjC Instance
#' @description Tests whether an object is an instance of an RObjC object class.
#'   Class and protocol generators return `FALSE`.
#' @param obj Any R object.
#' @return `TRUE` for an RObjC instance, `FALSE` otherwise.
#' @section Implementation details:
#' The check reads the instance's private `.__isa` field.
#' @examples
#' robjc_isRobjcObject(RSObject$new())
#' robjc_isRobjcObject(RSObject)
#' @export
robjc_isRobjcObject <- function(obj) {
  if (is.null(obj) || !is.environment(obj))
    return(FALSE) # not environment
  if (inherits(obj, ROBJC_CLASS))
    return(FALSE) # meta-class
  enclos <- obj$.__enclos_env__
  if (!is.environment(enclos) || !is.environment(enclos$private))
    return(FALSE)
  isa <- enclos$private$.__isa
  is.environment(isa) && inherits(isa, ISA_OBJECT)
}



# ------------
# Instance or MetaClass
# ------------
#' @title Check for an RObjC Instance or Generator
#' @description Tests whether an object is either an RObjC instance or an RObjC
#'   class or protocol generator.
#' @param obj Any R object.
#' @return `TRUE` for a recognized RObjC instance or generator, otherwise
#'   `FALSE`.
#' @export
robjc_isBelongingRobjc <- function(obj) {
  robjc_isMetaClass(obj) || robjc_isRobjcObject(obj)
}



# ------------
# Inherit chain
# ------------
#' @title Get a Class Inheritance Chain
#' @description Returns the class recorded in an RObjC identity record,
#'   followed by each of its parent classes up to the root.
#' @param isa The internal `robjc_isa` environment of a class or instance.
#' @return A list of class generators, starting with the class represented by
#'   `isa`.
#' @examples
#' robjc_inheritChain(RSObject$robjc_isa)
#' @export
robjc_inheritChain <- function(isa) {
  if (!is.environment(isa) || !inherits(isa, ISA_ROOT))
    stop("`isa` must be an RObjC identity record.", call. = FALSE)
  chain <- list(isa$class)
  klass <- isa$super
  while(!is.null(klass)) {
    chain[[length(chain) + 1L]] <- klass
    klass <- klass$robjc_isa$super
  }
  return(chain)
}



# ------------
# `hash()` & `isEqual()` insertion
# ------------
# Give every RObjC generator identity-based value methods. Use the address of
# the generator environment itself; get_pointer_hex() identifies the method
# environment and therefore does not serve this purpose. Map the bit pattern
# reserved for R's NA_integer_ to 0L so generator hashes remain non-missing.
robjc_install_meta_value_methods <- function(generator) {
  generator$isEqual <- function(other) identical(generator, other)
  generator$hash <- function() {
    address <- base::format.default(generator)
    hex <- sub("^<environment: 0x([[:xdigit:]]+)>$", "\\1", address)
    if (identical(hex, address))
      stop("Could not determine generator address.", call. = FALSE)
    hex <- paste0(strrep("0", max(0L, 8L - nchar(hex))), hex)
    hex <- substr(hex, nchar(hex) - 7L, nchar(hex))
    upper <- strtoi(substr(hex, 1L, 4L), base = 16L)
    lower <- strtoi(substr(hex, 5L, 8L), base = 16L)
    hash_code <- bitwOr(bitwShiftL(upper, 16L), lower)
    normalize_hash(hash_code)
  }
  # .onLoad() restores these locks for package-defined generators.
  lockBinding("isEqual", generator)
  lockBinding("hash", generator)
  invisible(generator)
}




# ------------
# Make `isa`
# ------------
#' @title Attach RObjC Identity to a Generator
#' @description Internal helper that creates and attaches an identity environment
#'   to a class or protocol generator and its private instance fields.
#' @param r6generator An R6 class generator to initialize.
#' @param super_class Its parent class or protocol generator.
#' @param protocols A list of adopted protocol generators.
#' @param slot Optional class-specific metadata.
#' @param version One character string giving the class version.
#' @return The new identity environment, invisibly.
#' @noRd
robjc_make_isa <- function(
  r6generator,
  super_class,
  protocols = list(),
  slot = NULL,
  version = "0.0.4 alpha"
) {
  if (!is.list(protocols) || !all(vapply(protocols, robjc_isProtocol, logical(1))))
    stop("`protocols` must be a list of RObjC protocols.", call. = FALSE)
  if (!is.character(version) || length(version) != 1L || is.na(version) || !nzchar(version))
    stop("`version` must be one non-empty character string.", call. = FALSE)
  if (grepl("_", version, fixed = TRUE))
    stop("Version string must not contain '_'.", call. = FALSE)
  if (is.null(super_class))
    stop("`super_class` cannot be NULL.", call. = FALSE)
  if (!inherits(super_class$robjc_isa, ISA_ROOT))
    stop(sprintf("%s does not inherit RSIsA", super_class$classname), call. = FALSE)
  isa <- list2env(
    list(
      class     = r6generator,
      super     = super_class,
      protocols = protocols,
      slot      = slot,
      version   = version
    ),
    envir = new.env(parent = emptyenv(), hash = FALSE)
  )
  # make isa S3
  if (inherits(super_class$robjc_isa, ISA_OBJECT))
    class(isa) <- c(ISA_OBJECT, ISA_ROOT)
  else if (inherits(super_class$robjc_isa, ISA_PROTOCOL))
    class(isa) <- c(ISA_PROTOCOL, ISA_ROOT)
  else
    stop("FATAL ERROR", call. = FALSE)
  r6generator$robjc_isa             <- isa
  r6generator$private_fields$.__isa <- isa
  # make generator S3
  class(r6generator) <- c(ROBJC_CLASS, R6GENERATOR)
  robjc_install_meta_value_methods(r6generator)
  invisible(isa)
}



# ------------
# Freeze `isa`
# ------------
#' @title Lock RObjC Class Identity
#' @description Internal helper that locks a generator's `robjc_isa`
#'   environment and its binding on the generator.
#' @param klass An initialized RObjC class or protocol generator.
#' @return `klass`, invisibly.
#' @noRd
robjc_freeze_isa <- function(klass) {
  lockEnvironment(klass$robjc_isa, bindings = TRUE)
  # .onLoad() restores these locks for package-defined generators and ISA records.
  lockBinding("robjc_isa", klass)
  invisible(klass)
}




# ------------
# Make singleton
# ------------
#' @title Attach Singleton Identity and Create Its Instance
#' @description Internal helper that initializes class identity, calls `$new()`
#'   without arguments, and stores the instance on the generator and in its
#'   identity record. The instance binding is locked.
#' @param r6generator An R6 class generator to initialize.
#' @param super_class Its parent RObjC object class generator.
#' @param protocols A list of adopted protocol generators.
#' @param version One character string giving the class version.
#' @noRd
robjc_make_singleton_isa <- function(
  r6generator,
  super_class,
  protocols = list(),
  version   = "0.0.4 alpha"
) {
  if (!inherits(super_class$robjc_isa, ISA_OBJECT))
    stop(
      sprintf("`super_class`: %s does not inherit RSObject", super_class$classname),
      call. = FALSE
    )
  robjc_make_isa(
    r6generator,
    super_class,
    protocols = protocols,
    slot      = NULL,
    version   = version
  )
  r6generator$robjc_isa$slot <- r6generator$new()
  r6generator$instance <- r6generator$robjc_isa$slot
  # .onLoad() restores this lock for package-defined singleton generators.
  lockBinding("instance", r6generator)
  class(r6generator) <- c(ROBJC_SINGLETON, ROBJC_CLASS, R6GENERATOR)
  invisible(r6generator$robjc_isa)
}
