#' @include RSModule.r
#' @include RSEncoding.r
#' @title RSClassDecoder
#' @description
#' `RSClassDecoder` is the decoding module in the `robjc` framework.
#' It interprets serialized metadata produced by `RSClassEncoder` and restores object identity and class bindings.
#' The decoder resolves class references using a dictionary that maps saved classIDs to runtime classes.
#'
#' This class conforms to the `RSDecoderProtocol`, and is typically invoked by functions like `robjc_read_rds()` or `robjc_load()`.
#'
#' @field class_store A dictionary containing class ID to instance mappings
#'   ([RSClassReference] entries), or `NULL` before data is loaded.
#' @field unresolved A character vector of classIDs that could not be resolved during decoding.
#'
#' @seealso [RSClassEncoder], [RSClassReference], [RSDecoderProtocol]
#'
#' @export
RSClassDecoder <- RSSingletonClass(
  "RSClassDecoder",
  version = "0.0.3 alpha",
  super_class = RSModule,
  protocols = list(RSDecoderProtocol),
  public = list(

    class_store = NULL,      # RSDictionary<String, RSClassReference>
    unresolved = NULL,       # character vector of unresolved classIDs



    #' @description
    #' Initializes the decoder with no encoded class store and an empty list of
    #' unresolved class IDs. The factory calls this method when creating `$instance`.
    #'
    #' @return The decoder instance, invisibly.
    initialize = function() {
      self$class_store <- NULL
      self$unresolved <- character(0)
      super$initialize()
      invisible(self)
    },




    #' @description
    #' Load encoded class map (from encoder) into internal state
    #' @param encoded_data A dictionary produced by RSClassEncoder$lockInstance()
    loadEncodedData = function(encoded_data) {
      stopifnot(inherits(encoded_data, "RSMutableDictionary"))
      self$class_store <- encoded_data
      self$unresolved <- character(0)
      invisible(self)
    },




    #' @description
    #' Return all encoded class mappings as key-value pairs.
    #' @return A list of `RSKeyValuePair` objects whose values are
    #'   `RSInstanceMap` objects.
    encodedClasses = function() {
      stopifnot(!is.null(self$class_store))
      return(self$class_store$allEntriesAsList())
    },




    #' @description
    #' Return list of classIDs from all stored RSClassReference entries.
    #' @return A list of classIDs
    encodedClassIDs = function() {
      vapply(
        self$encodedClasses(),
        function(entry) entry$key()$value(),
        character(1)
      )
    },




    #' @description
    #' Resolves saved classIDs in the encoded data using a provided mapping of classID strings
    #' to runtime R6Class objects. If a classID cannot be resolved, it is added to the `unresolved` field.
    #'
    #' For each resolved class, `resolveClassWith()` is called on all encoded instances to
    #' update their internal class pointer (`private$.__isa`).
    #'
    #' If any classes remain unresolved, the decoder will stop and raise an error with classID details.
    #'
    #' @param class_map An `RSDictionary` that maps classID strings (e.g., "MyClass_1.0") to R6Class objects.
    #'
    #' @return The decoder object (invisible).
    resolveClassesWithDictionary = function(class_map) {
      stopifnot(class_map$isKindOf(RSMutableDictionary))
      self$unresolved <- character(0)
      entries <- self$encodedClasses()
      for (entry in entries) {
        instance_map <- entry$value()
        class_id <- entry$key()$value()
        new_class    <- class_map$objectForKey(RSString_withValue(class_id))
        if (is.null(new_class)) {
          self$unresolved <- c(self$unresolved, class_id)
          next
        }
        instances <- instance_map$objects()$allObjectsAsList()
        for (obj in instances) {
          obj$resolveClassWith(new_class$classObject())
        }
      }
      if (length(self$unresolved) > 0) {
        msg <- sprintf(
          "Unresolved classIDs: %s",
          paste(self$unresolved, collapse = ", ")
        )
        stop(msg, call. = FALSE)
      }
      invisible(self)
    },




    #' @description
    #' Copies all decoded objects into the specified environment using their `objectID()` as variable names.
    #' This method is used to populate the global environment or a sandboxed scope after decoding.
    #'
    #' @param envir The environment into which objects should be unpacked. Default is `.GlobalEnv`.
    #'
    #' @return The decoder object (invisible).
    unpackTo = function(envir = .GlobalEnv) {
      stopifnot(!is.null(self$class_store))
      entries <- self$encodedClasses()
      for (entry in entries) {
        map <- entry$value()
        instances <- map$objects()$allObjectsAsList()
        for (obj in instances) {
          obj_name <- obj$objectID()
          assign(obj_name, obj, envir = envir)
        }
      }
      invisible(self)
    }
  )
)
