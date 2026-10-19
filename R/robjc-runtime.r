#' @include runtime_robjc.r
#' @include runtime_print.r
#' @include runtime_protocol.r
#' @include runtime_class.r
#' @include runtime_object.r
#' @include runtime_util.r
#' @include Rcpp.r
#' @include util_io.r
#'
#' @title Runtime Function Index
#'
#' @description
#' Reference for the functions in the `runtime_*` files. Functions marked
#' internal are documented for developers but are not exported.
#'
#' @section RObjC identity and ISA:
#' - [robjc_isMetaClass()] recognizes class and protocol generators.
#' - [robjc_isClass()] recognizes object class generators.
#' - [robjc_isProtocol()] recognizes protocol generators.
#' - [robjc_isRobjcObject()] recognizes instances.
#' - [robjc_isBelongingRobjc()] recognizes instances or generators.
#' - [robjc_inheritChain()] follows the classes in an ISA record.
#' - [R6GENERATOR] documents the runtime's type markers.
#' - RObjC generators expose `$isEqual(other)` for reference identity and
#'   `$hash()` for a non-missing integer hash derived from the generator
#'   environment address. The `NA_integer_` bit pattern maps to `0L`.
#' - `robjc_make_isa()`, `robjc_freeze_isa()`,
#'   `robjc_make_singleton_isa()`, and `protocol_make_isa()` construct and
#'   lock ISA records for the class factories.
#'
#' @section Class inspection (internal):
#' - [class_check()] validates a class generator.
#' - [class_getName()] returns its name.
#' - [class_isSingleton()] checks the singleton marker.
#' - [class_getSuperClass()] returns its direct parent.
#' - [class_getInstanceMethod()] searches for a public instance method.
#' - [class_getWholeProtocols()] collects adopted protocols across ancestors.
#'
#' @section Object inspection (internal):
#' - [object_getClass()] returns an instance's class generator.
#'
#' @section Protocol inspection (internal):
#' - [protocol_getInheritProtocols()] returns direct parent protocols.
#' - [protocol_getAllInheritProtocols()] returns all parent protocols.
#' - [protocol_conformsTo()] checks protocol inheritance.
#' - [protocol_getContract()] returns directly declared method names.
#' - [protocol_getAllInheritContracts()] returns inherited method names.
#' - [protocol_getAllContracts()] returns all declared method names.
#'
#' @section Environment identity (internal):
#' - [get_pointer_hex()] returns an in-session environment address.
#'
#' @section Printing:
#' - [format.RSObject()] and [print.RSObject()] display instances.
#' - [format.RSIsA()] and [print.RSIsA()] display class identity records.
#' - [format.RobjcClass()] and [print.RobjcClass()] display class generators.
#'
#' @seealso [robjc-package], [factory_api_index], [class_index]
#' @name runtime_functions
#' @aliases runtime
NULL # Keep this object for the roxygen include directives.
