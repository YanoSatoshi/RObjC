# Archived 0.0.4 encoder for reference only. This file is not loaded by robjc.
# It depends on the deleted class-reference and instance-map implementation.
# RSObject
#   └── RSWrappedClass
#
# OldClassEncoder$class_store : RSDictionary<RSString, RSClassReference>
#   └── RSClassReference -> RSKeyValuePair
#         ├─ key   = RSString;                         alias_getter: classID()
#         └─ value = RSInstanceMap -> RSKeyValuePair;  alias_getter: instanceMap()
#                     ├─ key   = RSWrappedClass;       alias_getter: wrappedClass()
#                     └─ value = RS**Set;              alias_getter: objects()
#
#
#' @include RSModule.r
#' @include RSCoding.r
#' @include RSMutableDictionary.r
#'
#' @title OldClassEncoder
#'
#' @description
#' `OldClassEncoder` is the core encoder module for robjc object serialization.
#' It implements the `RSEncoderProtocol` and is responsible for recursively encoding
#' robjc-compatible objects, recording their class structure and instance relationships.
#'
#' The encoder uses a mark-and-store traversal strategy, ensuring that each object and
#' class is encoded only once. The output is a dictionary of class references that can
#' be consumed by the `OldClassDecoder`.
#'
#' @field class_store
#' A dictionary that maps class IDs to [RSClassReference] objects. It is mutable
#' until `lockInstance()` and an immutable [RSDictionary] afterward.
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
#' @seealso [OldClassDecoder], [RSCoding], [RSClassReference]
#'
#' @export
OldClassEncoder <- RSSingletonClass(
  "OldClassEncoder",
  version = "0.1.0 beta",
  super_class = RSModule,
  protocols = list(RSEncoderProtocol),
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
    #' @param obj An object conforming to `RSObject` and `RSCoding`.
    #'
    #' @return The `RSInstanceMap` stored for the object's class.
    #' @export
    encodingContainer = function(obj) {
      if (self$status_frozen) stop("Encoder is frozen.", call. = FALSE)
      if (!robjc_isRobjcObject(obj))
        stop("`obj` must be an RObjC instance.", call. = FALSE)
      klass <- obj$class()
      version <- klass$robjc_isa$version
      stopifnot("Version string must not contain '_'." = !grepl("_", version))
      cls_name_var <- RSString_withValue(
        sprintf("%s_%s", klass$classname, version)
      )
      instance_map <- self$class_store$objectForKey(cls_name_var)
      if (is.null(instance_map)) {
        wrapped <- RSWrappedClass$new(klass)
        reference <- RSClassReference$new(wrapped, list())
        instance_map <- reference$instanceMap()
        assign(robjc_get_pointer(klass), TRUE, envir = self$visited_env)
        self$class_store$addEntry(reference)
      }
      return(instance_map)
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
      if (!robjc_isRobjcObject(obj))
        stop("`obj` must be an RObjC instance.", call. = FALSE)
      walk <- function(current) {
        oid <- current$objectID()
        if (exists(oid, envir = self$visited_env, inherits = FALSE))
          return(invisible(NULL))
        container <- self$encodingContainer(current)
        container$addObject(current)
        assign(oid, TRUE, envir = self$visited_env)
        for (child in current$objectsForEncoding()) walk(child)
        invisible(NULL)
      }
      tryCatch(
        walk(obj),
        error = function(e) {
          self$refresh()
          stop(e)
        }
      )
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
      if (self$status_frozen) return(self$class_store)
      instance_maps <- self$class_store$allValuesAsList()
      # prepare class list
      add_class_safely <- function(cls) {
        ## only for class use not instance use
        cls_id <- robjc_get_pointer(cls)
        if (exists(cls_id, envir = self$visited_env, inherits = FALSE))
          return(invisible(NULL))
        wrapped_cls <- RSWrappedClass$new(cls)
        if (self$class_store$containsKey(wrapped_cls$classID())) {
          assign(cls_id, TRUE, envir = self$visited_env)
          return(invisible(NULL))
        }
        class_ref   <- RSClassReference$new(wrapped_cls)
        self$class_store$addEntry(class_ref)
        assign(cls_id, TRUE, envir = self$visited_env)
      }
      for (map in instance_maps) {
        wrapped_class <- map$wrappedClass()
        super_classes <- robjc_inheritChain(
          wrapped_class$classObject()$robjc_isa
        )[-1L]
        for (super_cls in super_classes) add_class_safely(super_cls)
      }
      for (map in self$class_store$allValuesAsList()) map$lockInstance()
      self$class_store <- self$class_store$copy()
      self$status_frozen <- TRUE
      return(self$class_store)
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
