#' @include api_setup.r
#'
#' @title RSClassRegistry: Singleton Registry for robjc Standard Classes
#'
#' @description
#' `RSClassRegistry` is an internal singleton class that manages the list of class names
#' considered "standard" within the robjc framework. The current collector
#' selects exported RObjC class generators that inherit from `RSObject`.
#'
#' It provides a lookup of standard class names for class resolution. The
#' current save/load paths collect standard classes separately.
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
#'   \item{initialize()}{Constructs the registry; names are collected on first lookup.}
#'   \item{isStandardClass(classname)}{Returns TRUE if the given class name is a registered standard robjc class.}
#' }
#'
#' @seealso [robjc_load()], [robjc_read_rds()]
#'
#' @keywords internal
RSClassRegistry <- RSSingletonClass(
  "RSClassRegistry",
  version = "0.0.4 alpha",
  super_class = RSObject,
  private = list(
    .class_dictionary = NULL
  ),
  public = list(

    #' @description
    #' Initializes the `RSClassRegistry` singleton. Standard classes are collected
    #' on the first lookup, after the package has finished loading.
    initialize = function() {
      super$initialize()
      invisible(self)
    },


    #' @description
    #' Checks whether a given class name is registered as a standard robjc class.
    #' The first call collects standard class names after package loading.
    #'
    #' @param klass_name A character string representing the name of the class to check.
    #' @return TRUE if the class is a standard robjc class; FALSE otherwise.
    isStandardClass = function(klass_name) {
      if (is.null(private$.class_dictionary)) {
        classes <- get_standard_robjc_classes(as_class_name = FALSE)
        keys <- lapply(names(classes), RSString_withString)
        dictionary <- RSDictionary_withObjectsForKeys(
          objects = classes, keys = keys
        )
        private$.class_dictionary <- dictionary
      }
      !is.null(
        private$.class_dictionary$objectForKey(RSString_withString(klass_name))
      )
    }
  )
)
