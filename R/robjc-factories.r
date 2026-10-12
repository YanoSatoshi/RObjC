#' Factory API Index
#'
#' @description
#' The factory functions declare RObjC classes and protocols. Each returns an
#' R6 class generator. Choose the factory that matches the kind of class you
#' are declaring.
#'
#' @section Object classes:
#' - [RSObjectClass()] declares a regular object class.
#'
#' @section Specialized object classes:
#' - [RSSingletonClass()] declares a class and creates its shared instance.
#' - [RSScalarClass()] declares a scalar class and creates its shared NA
#'   companion instance.
#'
#' @section Protocols:
#' - [RSProtocolClass()] declares a protocol and its required methods.
#'
#' @seealso [robjc-package], [class_index], [runtime_functions]
#' @name factory_api_index
#' @aliases robjc_factories
NULL
