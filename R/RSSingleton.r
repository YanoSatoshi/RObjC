#! DEPRECATED

#' @include RSObject.r
#' @title RSSingleton: Legacy Singleton Base Class
#'
#' @description
#' `RSSingleton` is a legacy R6 subclass of [RSObject]. It remains available for
#' compatibility, but its class definition does not create or enforce a single
#' instance.
#'
#' New singleton classes are declared with `RSSingletonClass()`. That factory
#' creates an instance during class declaration and stores it in the generator's
#' `$instance` binding and `robjc_isa$slot`.
#'
#' The factory currently leaves `$new()` callable, so using `$instance` is
#' necessary when callers must share the same object.
#'
#' @export
RSSingleton <- R6::R6Class(
  "RSSingleton",
  class     = FALSE,
  cloneable = FALSE,
  portable  = TRUE,
  active    = NULL,
  inherit   = RSObject
)
