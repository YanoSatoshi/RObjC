#' @title class_check
#' @description TODO.
#' @noRd
class_check <- function(klass) {
  if (!robjc_isRobjcClass(klass)) stop("`klass` is not RklassC CLASS.")
  return(TRUE)
}


#' @title class_getName
#' @description TODO.
#' @noRd
#' @export
class_getName <- function(klass) {
  class_check(klass)
  return(klass$classname)
}


#' @title class_isSingleton
#' @description TODO.
#' @noRd
#' @export
class_isSingleton <- function(klass) {
  class_check(klass)
  return(inherits(klass, ROBJC_SINGLETON))
}


#' @title class_getSuperClass
#' @description TODO.
#' @noRd
#' @export
class_getSuperClass <- function(klass) {
  class_check(klass)
  return(klass$robjc_isa$super)
}


#' @title class_getInstanceMethod
#' @description TODO.
#' @noRd
#' @export
class_getInstanceMethod <- function(
  klass,
  method_name
) {
  class_check(klass)
  current <- klass
  while(!is.null(current)) {
    method <- current$public_methods[[method_name]]
    if (!is.null(method))
      return(method)
    current <- current$robjc_isa$super
  }
  return(NULL)
}


#' @title class_getWholeProtocols
#' @description TODO.
#' @noRd
#' @export
class_getWholeProtocols <- function(klass) {
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


# #' @export
# class_getInstanceVariable <- function(

# ) {

# }


# #' @export
# class_addMethod <- function() {

# }
