#' @include api_setup.r
#'
#' @title RSClassRegistry: Singleton Registry for robjc Standard Classes
#'
#' @description
#' `RSClassRegistry` is an internal singleton class that manages the list of class names
#' considered "standard" within the robjc framework. The current collector
#' selects exported R6 generators that inherit directly from `RSObject`.
#'
#' This registry is used during class resolution (e.g., in `robjc_load()` and `robjc_read_rds()`)
#' to determine whether a serialized object's class name should be looked up in the
#' robjc namespace or treated as a user-defined/global class.
#'
#' `RSSingletonClass()` creates the registry while package classes are loaded.
#'
#' @section Usage:
#' You can access the singleton instance via:
#' \preformatted{
#'   RSClassRegistry$instance
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{initialize()}{Constructs the registry and collects class names using `get_standard_robjc_classes()`.}
#'   \item{isStandardClass(classname)}{Returns TRUE if the given class name is a registered standard robjc class.}
#' }
#'
#' @seealso [robjc_load()], [robjc_read_rds()]
#'
#' @keywords internal
RSClassRegistry <- RSSingletonClass(
  "RSClassRegistry",
  version = "0.0.2 alpha",
  super_class = RSObject,
  public = list(

    #' @field std_classes
    #' A character vector containing the names of all robjc standard classes.
    #' These class names are automatically detected from the robjc namespace
    #' and include all classes that inherit directly from `RSObject`.
    #' This field is populated by `initialize()` via `get_standard_robjc_classes()`.
    std_classes = character(),


    #' @description
    #' Initializes the `RSClassRegistry` singleton. It populates the `std_classes` field
    #' by scanning the robjc namespace and collecting all exported R6 classes that inherit
    #' from `RSObject`. This ensures consistent resolution of standard class names during
    #' deserialization.
    initialize = function() {
      super$initialize()
      self$std_classes <- get_standard_robjc_classes(as_class_name = TRUE)
    },


    #' @description
    #' Checks whether a given class name is registered as a standard robjc class.
    #' This is used during deserialization to determine whether to resolve the class
    #' from the robjc namespace or from the global/user environment.
    #'
    #' @param klass_name A character string representing the name of the class to check.
    #' @return TRUE if the class is a standard robjc class; FALSE otherwise.
    isStandardClass = function(klass_name) {
      klass_name %in% self$std_classes
    }
  )
)
