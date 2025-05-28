# zzz.R — robjc package initializer
# This file defines the `.onLoad()` hook to register internal singletons such as RSClassRegistry.

.onLoad <- function(libname, pkgname) {
  register_rs_singleton(RSClassRegistry, RSClassRegistry$new())
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("robjc loaded - Objective-C inspired OOP system for R.")
  packageStartupMessage("welcom to robjc - version 0.0.1 alpha")
}