








as.selector <- function(sel_name) {
  stopifnot(is.character(sel_name), length(sel_name) == 1)
  structure(sel_name, class = "SEL")
}







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



# ### Protocols
# ## Already implemented partially.
# Protocol *objc_getProtocol(const char *name);

# Protocol **objc_copyProtocolList(unsigned int *outCount);

# const char *protocol_getName(Protocol *p);

# BOOL protocol_conformsToProtocol(Protocol *p, Protocol *other);

# struct objc_method_description protocol_getMethodDescription(Protocol *p, SEL aSel, BOOL isRequiredMethod, BOOL isInstanceMethod);
