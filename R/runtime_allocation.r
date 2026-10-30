###########################################################################?
# Portions of this file are derived from the R6 package.
#
# Copyright (c) 2015 RStudio
# Licensed under the MIT License.
#
# Source: https://github.com/r-lib/R6
# Original file: R/utils.R, R/new.R
#
# recursive_merge() modified for robjc class metadata.
#--------------------------------------------------------------------------

# Given two named vectors, join them together, and keep only the last element
# with a given name in the resulting vector. If b has any elements with the
# same name as elements in a, the element in a is dropped. Also, if there are
# any duplicated names in a or b, only the last one with that name is kept.
merge_vectors <- function(a, b) {
  if ((!is.null(a) && length(a) > 1 && is.null(names(a))) ||
      (!is.null(b) && length(b) > 1 && is.null(names(b)))) {
    stop("merge_vectors: vectors must be either NULL or named vectors")
  }

  x <- c(a, b)
  drop_idx <- duplicated(names(x), fromLast = TRUE)
  x[!drop_idx]
}


recursive_merge <- function(ClassObj, which) {
  if (is.null(ClassObj)) return(NULL)
  merge_vectors(
    recursive_merge(ClassObj$robjc_isa$super, which),
    ClassObj[[which]]
  )
}
###########################################################################?






#' @include primary_class_setup.r
#' @title Allocate an RObjC Instance Without Running Its Initializer
#' @description
#' Builds a new instance from an RObjC class generator, installing its inherited
#' public and private fields and methods without calling `initialize()`.
#' This is intended for archive restoration: the caller must subsequently
#' populate the logical state, for example through `initWithCoder()`.
#'
#' The allocated object is a fresh instance. Singleton generators return their
#' existing `$instance` instead.
#' @param ClassObj An RObjC object class generator.
#' @return An instance of `ClassObj`, invisibly.
#' @export
robjc_allocate <- function(ClassObj) {
  # util
  assign_methods2env <- function(methods, bind_env) {
    lapply(
      methods,
      function(fn) {environment(fn) <- bind_env; return(fn);}
    )
  }
  
  # Check robjc generator
  if (!robjc_isClass(ClassObj))
    stop("`ClassObj` must be an RObjC object class generator.", call. = FALSE)
  if (inherits(ClassObj, ROBJC_SINGLETON))
    return(ClassObj$instance)

  # Merge fields
  public_fields   <- merge_vectors(
    recursive_merge(ClassObj$robjc_isa$super, "public_fields"),
    ClassObj$public_fields
  )
  private_fields  <- merge_vectors(
    recursive_merge(ClassObj$robjc_isa$super, "private_fields"),
    ClassObj$private_fields
  )

  # Make bind_env
  private_bind_env  <- new.env(parent = emptyenv(), hash = FALSE)
  public_bind_env   <- new.env(parent = emptyenv(), hash = FALSE)

  # Setup super-class env
  inherit_chain <- rev(robjc_inheritChain(ClassObj$robjc_isa))  # [RSObject, Parent, Child]
  super_bind_env <- NULL
  accumulated_public_methods  <- list()
  accumulated_private_methods <- list()
  for (idx in seq_along(inherit_chain)) {
    Class <- inherit_chain[[idx]]
    # Setup super enclos env
    enclos_env  <- new.env(parent = Class$parent_env, hash = FALSE)
    enclos_env$self     <- public_bind_env
    enclos_env$private  <- private_bind_env
    enclos_env$super    <- super_bind_env
    public_methods  <- assign_methods2env(Class$public_methods,  enclos_env)
    private_methods <- assign_methods2env(Class$private_methods, enclos_env)

    # Setup super bind env
    bind_env  <- new.env(parent = emptyenv(),  hash = FALSE)
    bind_env$`.__enclos_env__` <- enclos_env

    # renew accumulators
    super_bind_env <- bind_env
    accumulated_public_methods  <- merge_vectors(
      accumulated_public_methods,
      public_methods
    )
    accumulated_private_methods <- merge_vectors(
      accumulated_private_methods,
      private_methods
    )
    if (length(accumulated_public_methods))
      list2env(accumulated_public_methods, bind_env)
    if (length(accumulated_private_methods))
      list2env(accumulated_private_methods, bind_env)
  }
  public_methods  <- accumulated_public_methods
  private_methods <- accumulated_private_methods
  
  # Setup public_bind_env, private_bind_env
  if (length(public_fields))
    list2env(public_fields, envir = public_bind_env)
  if (length(public_methods))
    list2env(public_methods, envir = public_bind_env)
  if (length(private_fields))
    list2env(private_fields, envir = private_bind_env)
  if (length(private_methods))
    list2env(private_methods, envir = private_bind_env)
  public_bind_env$`.__enclos_env__` <- enclos_env

  # Lock
  if (ClassObj$lock_objects) {
    lockEnvironment(public_bind_env)
    lockEnvironment(private_bind_env)
  }
  for (fn in names(public_methods))
    lockBinding(fn, public_bind_env)

  class(public_bind_env) <- vapply(
    rev(inherit_chain),
    function(cls) cls$classname,
    character(1)
  )

  # GC
  if (is.function(private_bind_env[["finalize"]])) {
    finalizer_wrapper <- function(e) {
      e$`.__enclos_env__`$private$finalize()
    }
    environment(finalizer_wrapper) <- baseenv()
    reg.finalizer(
      public_bind_env,
      finalizer_wrapper,
      onexit = TRUE
    )
  }
  if (is.function(public_bind_env[["finalize"]])) {
    finalizer_wrapper <- function(e) {
      e$`.__enclos_env__`$self$finalize()
    }
    environment(finalizer_wrapper) <- baseenv()
    reg.finalizer(
      public_bind_env,
      finalizer_wrapper,
      onexit = TRUE
    )
  }
  

  invisible(public_bind_env)
}


# Install the RObjC construction path after a generator has its identity record.
# `initialize()` determines the return value of `$new()`; the allocated object
# remains available as `self` within that method.
robjc_install_new <- function(generator) {
  if (!robjc_isClass(generator))
    stop("`generator` must be an RObjC object class generator.", call. = FALSE)
  generator$new <- local({
    klass <- generator
    function(...) {
      if (inherits(klass, ROBJC_SINGLETON))
        return(klass$instance)
      object <- robjc_allocate(klass)
      object$initialize(...)
    }
  })
  invisible(generator)
}

# RSObject is created directly with R6Class() before this file is loaded.
robjc_install_new(RSObject)
