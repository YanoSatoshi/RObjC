#' @include runtime_robjc.r


#' @title Validate an RObjC Class Generator
#' @description Internal guard for class introspection helpers.
#' @param klass A class generator to validate.
#' @return `TRUE` if validation succeeds; otherwise raises an error.
#' @keywords internal
class_check <- function(klass) {
  if (!robjc_isClass(klass))
    stop("`klass` is not RS_CLASS.", call. = FALSE)
  return(TRUE)
}



#' @title Get a Class Name
#' @description Returns the name assigned when an RObjC class was declared.
#' @param klass An RObjC class generator.
#' @return A character string containing the class name.
#' @keywords internal
class_getName <- function(klass) {
  class_check(klass)
  return(klass$classname)
}



#' @title Check Whether a Class Is a Singleton
#' @description Checks whether a class generator was declared as a singleton.
#' @param klass An RObjC class generator.
#' @return `TRUE` for a singleton class, `FALSE` otherwise.
#' @keywords internal
class_isSingleton <- function(klass) {
  class_check(klass)
  return(inherits(klass, ROBJC_SINGLETON))
}



#' @title Get the Parent Class
#' @description Returns the immediate parent of an RObjC class generator.
#' @param klass An RObjC class generator.
#' @return The parent class generator, or `NULL` for a root class.
#' @keywords internal
class_getSuperClass <- function(klass) {
  class_check(klass)
  return(klass$robjc_isa$super)
}



#' @title Find an Instance Method
#' @description Looks for a public method on a class, then walks up its parent
#'   classes until the method is found.
#' @param klass An RObjC class generator.
#' @param method_name One non-empty, non-missing character string naming the method.
#' @return The method function, or `NULL` when no class in the chain defines it.
#' @keywords internal
class_getInstanceMethod <- function(
  klass,
  method_name
) {
  class_check(klass)
  if (!is.character(method_name) ||
      length(method_name) != 1L ||
      is.na(method_name) ||
      !nzchar(method_name)) {
    stop("`method_name` must be one non-empty character string.", call. = FALSE)
  }
  current <- klass
  while(!is.null(current)) {
    method <- current$public_methods[[method_name]]
    if (!is.null(method))
      return(method)
    current <- current$robjc_isa$super
  }
  return(NULL)
}



#' @title Get Protocols Adopted by a Class and Its Parents
#' @description Collects the protocols recorded on a class and its ancestor
#'   classes, removing duplicate entries.
#' @param klass An RObjC class generator.
#' @return A list of protocol class generators.
#' @keywords internal
class_getWholeProtocols <- function(klass) {
  class_check(klass)
  protocols_list <- unlist(
    lapply(
      robjc_inheritChain(klass$robjc_isa),
      function(x) x$robjc_isa$protocols
    ),
    recursive = TRUE,
    use.names = FALSE
  )
  protocols_list[!duplicated(protocols_list)]
}