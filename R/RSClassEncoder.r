#' RSObject
#   └── RSWrappedClass
#
# RSClassEncoder$class_store : RSDictionary<RSString, RSClassReference>
#   └── RSClassReference -> RSKeyValuePair
#         ├─ key   = RSString;                         alias_getter: classID()      #nolint
#         └─ value = RSInstanceMap -> RSKeyValuePair;  alias_getter: instanceMap()  #nolint
#                     ├─ key   = RSWrappedClass;       alias_getter: wrappedClass() #nolint
#                     └─ value = RS**Set;              alias_getter: objects()      #nolint

#' @title RSClassEncoder
#'
#' @description
#' `RSClassEncoder` is the core encoder module for robjc object serialization.
#' It implements the `RSEncoderProtocol` and is responsible for recursively encoding
#' robjc-compatible objects, recording their class structure and instance relationships.
#'
#' The encoder uses a mark-and-store traversal strategy, ensuring that each object and
#' class is encoded only once. The output is a dictionary of class references that can
#' be consumed by the `RSClassDecoder`.
#'
#' @field class_store
#' A mutable `RSDictionary` that maps class IDs to `RSClassReference` objects.
#' Each entry represents a class and the set of encoded instances belonging to it.
#'
#' @field visited_env
#' An environment used to track visited objects and classes during recursive encoding.
#' Prevents re-encoding of already traversed objects (used for circular reference handling).
#'
#' @field status_frozen
#' A logical flag indicating whether the encoder has been finalized.
#' Once `lockInstance()` is called, the encoder is locked from further mutation.
#'
#' @seealso [RSClassDecoder], [RSEncoding], [RSClassReference]
#'
#' @export
RSClassEncoder <- R6::R6Class(
  "RSClassEncoder",
  inherit = RSModule,
  cloneable = FALSE,
  public = list(


    class_store = NULL, # RSMutableDictionary/RSDictionary


    visited_env = NULL,


    status_frozen = FALSE,


    
    #' @description
    #' Initializes the encoder. Creates a new mutable class store and a fresh environment
    #' to track visited objects and classes. This method is called automatically.
    #'
    #' @return The encoder instance (invisible).
    #' @export
    initialize = function() {
      self$class_store <- RSMutableDictionary$new()
      self$visited_env <- new.env(parent = emptyenv())
      super$initialize()
      invisible(self)
    },




    #' @description
    #' Returns or creates an `RSClassReference` container for the given object,
    #' indexed by class name and version. If not yet registered, the class is
    #' added to the encoder's store.
    #'
    #' @param obj An object conforming to `RSObject` and `RSEncoding`.
    #'
    #' @return An `RSClassReference` that holds the class and its instance set.
    #' @export
    encodingContainer = function(obj) {
      version <- tryCatch(obj$classVersion(), error = function(e) "0.0")
      stopifnot("Version string must not contain '_'." = !grepl("_", version))
      cls_name_var <- RSString$stringWith(
        sprintf("%s_%s", obj$class()$classname, version)
      )
      entry <- self$class_store$objectForKey(cls_name_var)
      if (is.null(entry)) {
        klass <- obj$class()
        wrapped <- RSWrappedClass$new(klass)
        entry <- RSClassReference$new(wrapped, list())
        assign(get_pointer_hex(klass), TRUE, envir = self$visited_env)
        self$class_store$addEntry(entry)
      }
      return(entry)
    },




    #' @description
    #' Recursively encodes an object and its dependent instances (as returned by
    #' `objectsForEncoding()`). The encoder skips already visited objects.
    #'
    #' @param obj An `RSObject` instance to encode.
    #'
    #' @return `NULL` (invisible). Side effects populate the encoder's store.
    #' @export
    encodeObject = function(obj) {
      stopifnot(!self$status_frozen)
      # obj: instance inherits RSObject
      oid <- obj$objectID()
      if (exists(oid, envir = self$visited_env, inherits = FALSE)) {
        return(invisible(NULL))
      }
      container <- self$encodingContainer(obj) # RSClassReference
      container$instanceMap()$objects()$addObject(obj)
      assign(oid, TRUE, envir = self$visited_env)
      for (child in obj$objectsForEncoding()) {
        self$encodeObject(child)
      }
    },




    #' @description
    #' Finalizes the encoding process by ensuring that all superclass hierarchies are registered
    #' and that the instance maps are locked. Returns a frozen `RSDictionary` representing
    #' the entire encoded structure.
    #'
    #' This method should be called before saving.
    #'
    #' @return A frozen `RSDictionary` of all encoded classes and their instances.
    #' @export
    lockInstance = function() {
      # get list of RSClassReferences.
      references <- self$class_store$allEntriesAsList()
      # get list of RSInstanceMaps.
      instance_maps <- lapply(references, function(ref) ref$instanceMap())
      # prepare class list
      add_class_safely <- function(cls) {
        ## only for class use not instance use
        cls_id <- get_pointer_hex(cls)
        if (exists(cls_id, envir = self$visited_env, inherits = FALSE)) return()
        wrapped_cls <- RSWrappedClass$new(cls)
        class_ref   <- RSClassReference$new(wrapped_cls)
        self$class_store$addEntry(class_ref)
        assign(cls_id, TRUE, envir = self$visited_env)
      }
      for (map in instance_maps) {
        wrapped_class <- map$wrappedClass()
        super_classes <- wrapped_class$classObject()$rs_inherit_chain
        for (super_cls in super_classes) add_class_safely(super_cls)
        map$lockInstance()  # lock the instance map
      }
      self$status_frozen <- TRUE
      return(self$class_store$copy())
    },




    #' @description
    #' Returns the finalized frozen data dictionary after `lockInstance()` has been called.
    #' An error is thrown if the encoder has not been finalized yet.
    #'
    #' @return An `RSDictionary` object.
    #' @export
    lockedData = function() {
      stopifnot("Encoder has not frozen." = self$status_frozen)
      return(self$class_store$copy())
    },




    #' @description
    #' Clears the encoder's internal state. If frozen, resets all slots for reuse.
    #' If not frozen, only the class store is cleared.
    #'
    #' This is useful for testing or repeated encoding passes.
    #'
    #' @return The encoder instance refreshed (invisible).
    #' @export
    refresh = function() {
      if (self$status_frozen) {
        # reset visited_env for reuse
        self$visited_env <- new.env(parent = emptyenv())
        self$class_store <- RSMutableDictionary$new()
        self$status_frozen <- FALSE
      } else {
        self$visited_env <- new.env(parent = emptyenv())
        self$class_store$removeAllEntries()
      }
      invisible(self)
    }
  )
)
register_rs_module(RSClassEncoder, RSClassEncoder$new())
conform_protocol_to_class(RSEncoderProtocol$protocol(), RSClassEncoder)
