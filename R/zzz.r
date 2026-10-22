# zzz.R — robjc package initializer
# R's package lazy-load database restores environments without their individual
# binding locks. Reapply locks for generators defined in this namespace.

.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  for (name in ls(ns, all.names = TRUE)) {
    generator <- get(name, envir = ns, inherits = FALSE)
    if (!robjc_isMetaClass(generator)) next

    lockEnvironment(generator$robjc_isa, bindings = TRUE)
    for (binding in c("robjc_isa", "isEqual", "hash", "instance",
                      "isNAClass", "na", "contract_methods", "null",
                      "is_na_class")) {
      if (exists(binding, envir = generator, inherits = FALSE))
        lockBinding(binding, generator)
    }
    if ((robjc_isProtocol(generator) && is.null(generator$new)) ||
        (inherits(generator, ROBJC_SINGLETON) && isTRUE(generator$isNAClass))) {
      lockBinding("new", generator)
    }
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("robjc loaded - Objective-C inspired OOP system for R.")
  packageStartupMessage("welcom to robjc - version 0.0.4 alpha")
}
