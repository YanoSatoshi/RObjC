#' @title Get standard robjc classes defined in the package
#'
#' @description
#' Scans the robjc package namespace and returns a list of standard robjc classes
#' that inherit from `RSObject`. These classes are considered part of the core
#' robjc framework and are used internally by the encoder and decoder modules.
#'
#' This function is primarily intended to support automatic class resolution during
#' deserialization. It can return either a vector of class names or a named list
#' of class objects, depending on the `as_class_name` argument.
#'
#' @param as_class_name Logical. If `TRUE` (default), returns a character vector of
#' class names. If `FALSE`, returns a named list of `R6ClassGenerator` objects.
#'
#' @return A character vector or a named list, depending on `as_class_name`.
#'
#' @examples
#' get_standard_robjc_classes()          # -> c("RSObject", "RSArray", ...)
#' get_standard_robjc_classes(FALSE)[["RSArray"]]  # -> RSArray class object
#'
#' @seealso [RSClassRegistry], [RSClassDecoder], [robjc_read_rds()]
#'
#' @export
get_standard_robjc_classes <- function(as_class_name = TRUE) {
  ns <- asNamespace("robjc")
  exported <- getNamespaceExports("robjc")
  generators <- Filter(function(name) {
    obj <- tryCatch(
      get(name, envir = ns, inherits = FALSE),
      error = function(e) NULL
    )
    inherits(obj, "R6ClassGenerator")
  }, exported)
  std_classes <- Filter(function(name) {
    klass <- get(name, envir = ns)
    tryCatch({
      inherits(klass$get_inherit(), "R6ClassGenerator") &&
        identical(klass$get_inherit()$classname, "RSObject")
    }, error = function(e) FALSE)
  }, generators)
  if (as_class_name) {
    return(unname(std_classes))
  } else {
    cls_objects <- lapply(
      std_classes,
      function(nm) {
        cls <- list(get(nm, envir = ns))
        names(cls) <- nm
        return(cls)
      }
    )
    return(cls_objects)
  }
}










#' @title Resolve Superclass Chain and Return Inheritance Metadata
#'
#' @description
#' Recursively traverses the `inherit` field of the given R6 class and constructs
#' an ordered list of ancestor classes (including the class itself and its superclasses).
#'
#' This function is used internally by the `RSObject` base initializer to populate
#' the `inherit_chain` field within the `robjc_isa` environment.
#' 
#' In typical use, users do not need to call this function directly, as the
#' inheritance metadata is automatically resolved and stored during object construction.
#' Advanced users may call this manually to inspect or manipulate class hierarchies.
#'
#' @param klass An R6 class object whose inheritance chain will be resolved.
#'
#' @return A list of R6 class objects representing the full inheritance chain,
#'         ordered from the current class up to the root `RSObject` class.
#'
#' @seealso
#' \code{\link{RSObject}} – The base class in the robjc system.
#' Its \code{initialize} method automatically invokes this function to
#' resolve and register inheritance metadata via \code{robjc_isa}.
#'
#' @keywords internal
resolve_class_ancestors <- function(klass) {
  cls <- klass
  chain <- list()
  while (!is.null(cls)) {
    chain[[length(chain) + 1]] <- cls
    if (is.symbol(cls$inherit) || is.call(cls$inherit)) {
      cls <- tryCatch(
        eval(cls$inherit, envir = environment(cls$public_methods$initialize)),
        error = function(e) {
          msg <- sprintf(
            "Failed to resolve superclass '%s' for class '%s'.",
            deparse(cls$inherit), cls$classname
          )
          stop(msg, call. = FALSE)
        }
      )
    } else {
      cls <- cls$inherit
    }
  }
  return(chain)
}








#' @title Inherit Protocol Definitions from Superclass
#'
#' @description
#' Retrieves the protocol conformance list from the superclass of the given robjc class
#' and returns it as a list of protocol references. This list is typically assigned to
#' the `protocols` field within the class's internal `robjc_isa` structure.
#'
#' If the given class is the base `RSObject`, this function returns the default
#' protocol list containing the core `RSEncoding` protocol.
#'
#' This function is used internally during class initialization to ensure that
#' protocol inheritance is preserved across subclasses.
#'
#' In typical usage, users do not need to call this function directly. It is invoked
#' automatically during `RSObject$initialize()` and stored in `robjc_isa$protocols`.
#'
#' @param klass An R6 class object whose protocol ancestry should be resolved.
#'
#' @return A list of protocol objects inherited from the superclass.
#'
#' @seealso
#' \code{\link{RSObject}} – The base class in the robjc system.
#' Its \code{initialize} method uses this function to assign protocol inheritance
#' metadata into \code{robjc_isa}.
#'
#' @keywords internal
resolve_protocol_ancestors <- function(klass) {
  klass$robjc_isa$super_class$robjc_isa$protocols
}









#' @title Determine Compatibility Between Encoded and Current Class
#'
#' @description
#' Checks whether a class used for encoding serialized data (`old_cls`) is compatible
#' with a currently loaded class definition (`new_cls`) for decoding.
#'
#' This compatibility check is based on the class name and version as stored in the
#' `robjc_isa` metadata of each class. Both must match exactly for the function to
#' return `TRUE`.
#'
#' This function is intended to ensure safe deserialization when objects are read
#' from persistent storage or transferred between sessions.
#'
#' @param old_cls The class object used at the time of serialization.
#' @param new_cls The class object currently loaded and used for deserialization.
#'
#' @return Logical `TRUE` if the classes are deemed compatible for decoding,
#'         `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#' old <- MyClass
#' old$robjc_isa$version <- "1.0"
#' new <- MyClass
#' new$robjc_isa$version <- "1.0"
#' can_resolve_class(old, new)  # TRUE
#' }
#'
#' @note
#' This function currently performs strict matching by class name and version.
#' Future extensions may support more flexible compatibility checks, such as
#' structural subsetting or explicit migration strategies.
#'
#' @seealso
#' \code{\link{RSObject}} – The base class that implements \code{resolveClassWith()} for
#' version-based decoding logic.
#'
#' @keywords internal
can_resolve_class <- function(old_cls, new_cls) {
  old_id <- sprintf("%s_%s", old_cls$classname, old_cls$robjc_isa$version)
  new_id <- sprintf("%s_%s", new_cls$classname, new_cls$robjc_isa$version)
  return(old_id == new_id)
}


.prepare_native_collections_for_serialization <- function(metadata) {
  for (instance_map in metadata$allValuesAsList()) {
    objects <- instance_map$objects()
    for (object in objects$allObjectsAsList()) {
      if (object$isKindOf(RSMutableDictionary) ||
          object$isKindOf(RSMutableSet)) {
        object$prepareForSerialization()
      }
    }
    objects$prepareForSerialization()
  }
  metadata$prepareForSerialization()
  invisible(metadata)
}








#' @title Internal utility to encode and save robjc objects
#'
#' @description
#' Given a named list of objects, this function encodes them using RSClassEncoder
#' and saves them along with robjc metadata using base::save().
#'
#' @param objects A named list of objects to be saved.
#' @param file File name to save to.
#' @param ascii Logical; save as ASCII?
#' @param version RDS version.
#' @param compress Compression setting.
#' @param compression_level Compression level setting.
#'
#' @return Invisibly returns TRUE on success.
#'
#' @keywords internal
robjc_encode_and_save <- function(objects,
                                  file,
                                  ascii = FALSE,
                                  version = NULL,
                                  compress = TRUE,
                                  compression_level = 6) {
  stopifnot(is.list(objects), is.character(names(objects)))
  # robjc encode
  encoder <- RSClassEncoder$module()$refresh()
  for (obj in objects) {
    if (robjc_isRobjcObject(obj)) encoder$encodeObject(obj)
  }
  metadata <- encoder$lockInstance()
  .prepare_native_collections_for_serialization(metadata)
  # construct save env
  save_env <- list2env(
    c(objects, list(.__rs_encoded_data__ = metadata)),
    parent = emptyenv()
  )
  # save using base::save
  save(
    list = ls(save_env, all.names = TRUE),
    file = file,
    envir = save_env,
    ascii = ascii,
    version = version,
    compress = compress,
    compression_level = compression_level
  )
  invisible(TRUE)
}







#' @title Save R objects with robjc metadata
#'
#' @description
#' A wrapper around `base::save()` that also stores robjc structural metadata.
#' This function behaves like `save()` but includes encoded metadata needed
#' to restore robjc objects.
#'
#' @inheritParams base::save
#'
#' @seealso [robjc_save_image()], [base::save()]
#'
#' @export
robjc_save <- function(...,
                       list = character(),
                       envir = parent.frame(),
                       file,
                       ascii = FALSE,
                       version = NULL,
                       compress = TRUE,
                       compression_level,
                       eval.promises = TRUE,
                       precheck = TRUE) {
  if (missing(file)) stop("`file` must be specified.")
  dot_names <- as.character(substitute(list(...)))[-1L]
  all_names <- unique(c(dot_names, list))
  if (length(all_names) == 0L) stop("No objects specified to save.")
  objects <- mget(all_names, envir = envir, inherits = FALSE)
  robjc_encode_and_save(
    objects = objects,
    file = file,
    ascii = ascii,
    version = version,
    compress = compress,
    compression_level = compression_level
  )
}








#' @title Save entire workspace with robjc encoding
#'
#' @description
#' Saves all objects in the given environment along with robjc metadata.
#' Equivalent to `save.image()` but compatible with robjc framework.
#'
#' @param file File to save to (default: "RData")
#' @param envir Environment to save (default: `.GlobalEnv`)
#' @param ascii Passed to `save()`
#' @param version Passed to `save()`
#' @param compress Passed to `save()`
#' @param safe Reserved for compatibility. It currently has no effect.
#'
#' @seealso [robjc_save()], [base::save.image()]
#'
#' @export
robjc_save_image <- function(file = "RData",
                             envir = .GlobalEnv,
                             ascii = FALSE,
                             version = NULL,
                             compress = !ascii,
                             safe = TRUE) {
  stopifnot(is.character(file), is.environment(envir))
  obj_names <- ls(envir = envir, all.names = TRUE)
  if (length(obj_names) == 0L) {
    warning("No objects to save in the environment.")
    return(invisible(TRUE))
  }
  objects <- mget(obj_names, envir = envir, inherits = FALSE)
  robjc_encode_and_save(
    objects = objects,
    file = file,
    ascii = ascii,
    version = version,
    compress = compress,
    compression_level = 6
  )
}






#' @title Save a single R object with robjc metadata to RDS
#'
#' @description
#' Wraps `base::saveRDS()` and attaches robjc encoding metadata alongside the object.
#' This enables the object to be restored later with full class and instance structure.
#'
#' The object must conform to the `RSEncoding` protocol. The function automatically
#' encodes the object and includes all reachable robjc objects.
#'
#' The resulting RDS file contains a list with two elements:
#' - `__object__`: The actual object to be restored
#' - `__robjc__`: Metadata for decoding the object
#'
#' @param object The object to be saved.
#' @param file File path to write to. Passed to `base::saveRDS()`.
#' @param ascii Logical; whether to save in ASCII format. Passed to `base::saveRDS()`.
#' @param version RDS format version. Passed to `base::saveRDS()`.
#' @param compress Compression method. Passed to `base::saveRDS()`.
#' @param refhook Passed to `base::saveRDS()`.
#'
#' @return Invisibly returns TRUE on success.
#'
#' @seealso [robjc_read_rds()], [base::saveRDS()]
#'
#' @export
robjc_save_rds <- function(object,
                           file,
                           ascii = FALSE,
                           version = NULL,
                           compress = TRUE,
                           refhook = NULL) {
  stopifnot(!missing(file))
  encoder <- RSClassEncoder$module()
  encoder$refresh()
  encoder$encodeObject(object)
  metadata <- encoder$lockInstance()
  .prepare_native_collections_for_serialization(metadata)
  wrapped <- list(
    `__object__` = object,
    `__robjc__`  = metadata
  )
  saveRDS(wrapped,
          file = file,
          ascii = ascii,
          version = version,
          compress = compress,
          refhook = refhook)
  invisible(TRUE)
}






#' @title Read RDS file with robjc metadata and restore object
#'
#' @description
#' Reads an RDS file saved by `robjc_save_rds()`, restores class metadata using
#' `RSClassDecoder`, and returns the fully reconstructed object.
#'
#' Standard robjc classes (e.g., RSObject, RSArray) are resolved automatically.
#' Users may also provide a `class_map` for resolving custom classes.
#' If some class IDs remain unresolved, a warning with class names will be shown.
#'
#' @param file Path to the `.rds` file to read.
#' @param class_map Optional `RSDictionary` of custom classID → class mappings.
#' @param refhook Optional function passed to `base::readRDS()`.
#'
#' @return Restored object with robjc class structure resolved.
#'
#' @seealso [robjc_save_rds()], [robjc_load()], [get_standard_robjc_classes()]
#'
#' @export
robjc_read_rds <- function(file,
                           class_map = NULL,
                           refhook = NULL) {
  stopifnot(file.exists(file))
  data <- readRDS(file = file, refhook = refhook)
  if (!is.list(data) || !all(c("__object__", "__robjc__") %in% names(data))) {
    stop("Invalid robjc RDS format: missing __object__ or __robjc__.")
  }
  # --- STEP 1: Generate standard class dictionary ---
  std_classes <- get_standard_robjc_classes(FALSE)
  std_keys <- lapply(
    std_classes,
    function(cls) {
      RSString$stringWith(
        paste(cls$classname, cls$robjc_isa$version, sep = "_")
      )
    }
  )
  std_map <- RSDictionary$dictionaryWithKeysAndObjects(
    keys = std_keys,
    objects = unname(std_classes)
  )
  # --- STEP 2: Merge std_map and class_map ---
  if (!is.null(class_map)) {
    stopifnot(inherits(class_map, "RSDictionary"))
    std_map <- std_map$dictionaryByMergingWithDictionary(class_map)
  }
  # --- STEP 3: Operate decoding ---
  decoder <- RSClassDecoder$module()
  decoder$loadEncodedData(data$`__robjc__`)
  decoder$resolveClassesWithDictionary(std_map)
  # --- Warn the unresolved classes exist. ---
  unresolved <- decoder$unresolved
  if (length(unresolved) > 0) {
    warning(sprintf(
      "The following class IDs could not be resolved:\n  - %s",
      paste(unresolved, collapse = "\n  - ")
    ))
  }
  return(data$`__object__`)
}








#' @title Load .rda file and decode robjc metadata if present
#'
#' @description
#' Wraps `base::load()` to support restoration of robjc-encoded objects.
#' If the `.rda` file contains a variable named `.__rs_encoded_data__`, this function
#' will automatically invoke the robjc decoder to restore class structures.
#'
#' Standard robjc classes are resolved automatically. Users may provide a `class_map`
#' for resolving additional custom classes. If any class IDs remain unresolved,
#' a warning will be displayed.
#'
#' @param file Path to the `.rda` file.
#' @param class_map Optional `RSDictionary` of custom classID → class mappings.
#' @param envir Environment in which to load the objects (default: `.GlobalEnv`).
#' @param verbose Logical. Print names of loaded objects?
#' @param hookFunction Optional hook passed to `base::load()`.
#' @param ... Additional arguments passed to `base::load()`.
#'
#' @return A character vector of names of objects created.
#'
#' @seealso [robjc_save()], [robjc_read_rds()]
#'
#' @export
robjc_load <- function(file,
                       class_map = NULL,
                       envir = .GlobalEnv,
                       verbose = FALSE,
                       hookFunction = NULL,
                       ...) {
  stopifnot(file.exists(file))
  # STEP 1: load all objects into isolated env
  temp_env <- new.env(parent = emptyenv())
  loaded_names <- base::load(file = file, envir = temp_env,
                             verbose = verbose,
                             hookFunction = hookFunction, ...)
  # STEP 2: detect and extract metadata
  if ("__rs_encoded_data__" %in% loaded_names) {
    metadata <- temp_env$.__rs_encoded_data__
    if (!inherits(metadata, "RSDictionary")) {
      stop(".__rs_encoded_data__ must be a RSDictionary.")
    }
    # Standard class dictionary.
    std_classes <- get_standard_robjc_classes(FALSE)
    std_keys <- lapply(
      std_classes,
      function(cls) {
        RSString$stringWith(
          paste(cls$classname, cls$robjc_isa$version, sep = "_")
        )
      }
    )
    std_map <- RSDictionary$dictionaryWithKeysAndObjects(
      keys = std_keys,
      objects = unname(std_classes)
    )
    # Custom class dictionary
    if (!is.null(class_map)) {
      stopifnot(inherits(class_map, "RSDictionary"))
      std_map <- std_map$dictionaryByMergingWithDictionary(class_map)
    }
    # Operate decoding
    decoder <- RSClassDecoder$module()
    decoder$loadEncodedData(metadata)
    decoder$resolveClassesWithDictionary(std_map)
    # Warns unresolved class exists.
    unresolved <- decoder$unresolved
    if (length(unresolved) > 0) {
      warning(sprintf(
        "The following class IDs could not be resolved:\n  - %s",
        paste(unresolved, collapse = "\n  - ")
      ))
    }
    # remove meta-data
    rm(list = ".__rs_encoded_data__", envir = temp_env)
    loaded_names <- setdiff(loaded_names, ".__rs_encoded_data__")
  }
  # STEP 3: output
  for (name in loaded_names) {
    assign(name, temp_env[[name]], envir = envir)
  }
  invisible(loaded_names)
}
