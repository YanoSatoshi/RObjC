#'
#' @keywords internal
ISA_ROOT      <- "RSIsA"

#'
#' @keywords internal
ISA_OBJECT    <- "RSObjectIsA"

#'
#' @keywords internal
ISA_PROTOCOL  <- "RSProtocolIsA"

lockBinding("ISA_ROOT",     environment())
lockBinding("ISA_OBJECT",   environment())
lockBinding("ISA_PROTOCOL", environment())

#'
#' @export
R6GENERATOR     <- "R6ClassGenerator"
ROBJC_CLASS     <- "RobjcClass"
ROBJC_SINGLETON <- "RobjcSingleton"
lockBinding("R6GENERATOR",      environment())
lockBinding("ROBJC_CLASS",      environment())
lockBinding("ROBJC_SINGLETON",  environment())

# MetaClass
#' @title Check if an Object is a robjc Class Object
#'
#' @description
#' Determines whether the given object is a class definition created within the `robjc` object-oriented system.
#' Specifically, it checks for the presence of the internal `robjc_isa` metadata structure,
#' which is a characteristic of all robjc class objects.
#'
#' @param obj Any R object.
#'
#' @return `TRUE` if the object is recognized as a robjc class object, `FALSE` otherwise.
#'
#' @examples
#' cls <- RSObject
#' robjc_isMetaClass(cls)  # Returns TRUE
#' robjc_isMetaClass(R6::R6Class("NotRobjc"))  # Returns FALSE
#'
#' @export
robjc_isMetaClass <- function(obj) {
  is.environment(obj) &&
    inherits(obj, ROBJC_CLASS) &&
    !is.null(obj$robjc_isa) &&
    inherits(obj$robjc_isa, ISA_ROOT)
}


# MetaClass ClassObj
#' @title robjc_isClass
#' @description TODO.
#' @noRd
#' @export
robjc_isClass <- function(obj) {
  robjc_isMetaClass(obj) &&
    inherits(obj$robjc_isa, ISA_OBJECT)
}


# MetaClass ProtocolObj
#' @title robjc_isProtocol
#' @description TODO.
#' @noRd
#' @export
robjc_isProtocol <- function(obj) {
  robjc_isMetaClass(obj) &&
    inherits(obj$robjc_isa, ISA_PROTOCOL)
}


# Instance
#' @title robjc_isRobjcObject
#' @description TODO.
#' @noRd
#' @export
robjc_isRobjcObject <- function(obj) {
  if (is.null(obj) || !is.environment(obj)) return(FALSE) # not environment
  if (inherits(obj, ROBJC_CLASS))           return(FALSE) # meta-class
  enclos <- obj$.__enclos_env__
  if (is.null(enclos) || is.null(enclos$private)) return(FALSE) # not instance
  isa <- enclos$private$.__isa
  !is.null(isa) && inherits(isa, ISA_OBJECT) # confirm
}


# Instance or MetaClass
#' @title robjc_isBelongingRobjc
#' @description TODO.
#' @noRd
#' @export
robjc_isBelongingRobjc <- function(obj) {
  robjc_isMetaClass(obj) || robjc_isRobjcObject(obj)
}


#' @title robjc_inheritChain
#' @description TODO.
#' @noRd
#' @export
robjc_inheritChain <- function(isa) {
  chain <- list(isa$class)
  klass <- isa$super
  while(!is.null(klass)) {
    chain[[length(chain) + 1L]] <- klass
    klass <- klass$robjc_isa$super
  }
  return(chain)
}


#' @title robjc_make_isa
#' @description TODO.
#' @noRd
robjc_make_isa <- function(
  r6generator,
  super_class,
  protocols = list(),
  slot = NULL,
  version = "0.0.1 alpha"
) {
  stopifnot(
    "`super_class` cannot be NULL." =
      !is.null(super_class),
    "`version` must be a character (length:1L)." =
      is.character(version) && length(version) == 1L
  )
  if (!inherits(super_class$robjc_isa, ISA_ROOT))
    stop(
      sprintf("%s does not inherit RSIsA", super_class$classname),
      call. = FALSE
    )
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
    stop("FATAL ERROR")
  r6generator$robjc_isa             <- isa
  r6generator$private_fields$.__isa <- isa
  # make generator S3
  class(r6generator) <- c(ROBJC_CLASS, R6GENERATOR)
  invisible(isa)
}


#' @title robjc_freeze_isa
#' @description TODO.
#' @noRd
robjc_freeze_isa <- function(klass) {
  lockEnvironment(klass$robjc_isa, bindings = TRUE)
  lockBinding("robjc_isa", klass)
  invisible(klass)
}






#' @title robjc_make_singleton_isa
#' @description TODO.
#' @noRd
robjc_make_singleton_isa <- function(
  r6generator,
  super_class,
  protocols = list(),
  version   = "0.0.1 alpha"
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
  lockBinding("instance", r6generator)
  ## deactivate $new
  # r6generator$._constractor <- r6generator$new
  # r6generator$new <- function(...) {
  #   stop("Cannot instantiate a Singleton directly.", call. = FALSE)
  # }
  # lockBinding("new", r6generator)
  class(r6generator) <- c(ROBJC_SINGLETON, ROBJC_CLASS, R6GENERATOR)
  invisible(isa)
}






# as.selector <- function(sel_name) {
#   stopifnot(is.character(sel_name), length(sel_name) == 1)
#   structure(sel_name, class = "SEL")
# }







# ### Class
# Class robjc_getClass(const char *name);
# Class robjc_getMetaClass(const char *name);
## R6Generator robjc_get_class(character);  # no metaclass in robjc. Class & MetaClass are the same.

# # no implement
# Class robjc_allocateClassPair(Class superclass, const char *name, size_t extraBytes);
# # no implement
# void robjc_registerClassPair(Class cls);
## Do not know when need.


# const char *class_getName(Class cls);
## use `cls$classname` instead

# Class class_getSuperclass(Class cls);
## use `cls$robjc_isa$super_class` instead

# BOOL class_isMetaClass(Class cls);
## use `!is.null(cls$robjc_isa)` instead. Will be functionalize.

# size_t class_getInstanceSize(Class cls);
## Currently no idea. Does anyone need?

# ### Methods
# RSMethod <- R6::R6Class("RSMethod", public = SEL, TypeEncoding, IMP)
# 
#
# Method class_getInstanceMethod(Class cls, SEL name);
# cls$public_methods[[name]], cls$private_methods[[name]], cls$injection$methods[[name]]
# `cls$injection` is for variant/method injecting environment.

# Method class_getClassMethod(Class cls, SEL name);
# `cls$.meta` will be implement.
# .meta$class_vars
# .meta$class_methods

# IMP method_getImplementation(Method m);
# 


# SEL method_getName(Method m);

# const char *method_getTypeEncoding(Method m);

# IMP class_getMethodImplementation(Class cls, SEL name);

# BOOL class_addMethod(Class cls, SEL name, IMP imp, const char *types);

# IMP class_replaceMethod(Class cls, SEL name, IMP imp, const char *types);


# ### Properties: @property readonly etc.
# ##  Never implements under current R6Class
# objc_property_t class_getProperty(Class cls, const char *name);

# objc_property_t *class_copyPropertyList(Class cls, unsigned int *outCount);

# const char *property_getName(objc_property_t property);

# const char *property_getAttributes(objc_property_t property);



# ### Ivars
# Ivar class_getInstanceVariable(Class cls, const char *name);

# Ivar class_getClassVariable(Class cls, const char *name);

# const char *ivar_getName(Ivar v);

## very difficult.
# const char *ivar_getTypeEncoding(Ivar v);

# ptrdiff_t ivar_getOffset(Ivar v);
# rubicon-objc.readthedocs.io




# ### SELs
# SEL sel_registerName(const char *str);

# const char *sel_getName(SEL sel);

# BOOL sel_isEqual(SEL lhs, SEL rhs);


