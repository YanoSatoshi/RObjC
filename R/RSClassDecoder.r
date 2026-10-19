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
#' @field class_store A dictionary mapping [RSString] class IDs to
#'   [RSInstanceMap] objects, or `NULL` before data is loaded.
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

    class_store = NULL,      # RSDictionary<RSString, RSInstanceMap>
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
      # A restored generator differs by reference from the current generator.
      if (!robjc_isRobjcObject(encoded_data) || !inherits(encoded_data, "RSMutableDictionary"))
        stop("`encoded_data` must be a dictionary.", call. = FALSE)
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
      #! validation check V-103
      #todo 現在の文書は list と書くが、vapply(..., character(1)) の返り値は character vector。空のときも character(0)。
      #? @return を「class ID の character vector」とし、説明も RSClassReference entry に依存しない表現へ直す。
      vapply(
        self$encodedClasses(),
        function(entry) entry$key()$value(),
        character(1)
      )
    },




    #' @description
    #' Resolves saved classIDs using a dictionary that maps [RSString] class IDs
    #' to [RSWrappedClass] objects. If a classID cannot be resolved, it is added to the `unresolved` field.
    #'
    #' For each resolved class, `resolveClassWith()` is called on all encoded instances to
    #' update their internal class pointer (`private$.__isa`).
    #'
    #' Class entries without instances record ancestry only and do not need a
    #' runtime class mapping. If any class with instances remains unresolved,
    #' the decoder stops with the missing class IDs.
    #'
    #' @param class_map An `RSDictionary` or `RSMutableDictionary` mapping
    #'   [RSString] class IDs (e.g., "MyClass_1.0") to matching [RSWrappedClass] objects.
    #'
    #' @return The decoder object (invisible).
    resolveClassesWithDictionary = function(class_map) {
      if (!robjc_isRobjcObject(class_map) || !class_map$isKindOf(RSMutableDictionary))
        stop("`class_map` must be a dictionary.", call. = FALSE)
      for (mapping in class_map$allEntriesAsList()) {
        key <- mapping$key()
        value <- mapping$value()
        if (!robjc_isRobjcObject(key) || !key$isKindOf(RSString) ||
            !robjc_isRobjcObject(value) || !value$isKindOf(RSWrappedClass) ||
            !robjc_isClass(value$classObject()) ||
            !identical(key$value(), value$classID()$value())) {
          stop(
            "`class_map` must map class IDs to matching RSWrappedClass objects.",
            call. = FALSE
          )
        }
      }
      self$unresolved <- character(0)
      entries <- self$encodedClasses()
      resolved <- lapply(
        entries,
        # Rebuild the key with the current RSString generator before lookup.
        function(entry) class_map$objectForKey(RSString_withValue(entry$key()$value()))
      )
      instances <- lapply(
        entries,
        function(entry) entry$value()$objects()$allObjectsAsList()
      )
      missing <- vapply(
        seq_along(entries),
        function(i) is.null(resolved[[i]]) && length(instances[[i]]) > 0L,
        logical(1)
      )
      self$unresolved <- vapply(
        entries[missing],
        function(entry) entry$key()$value(),
        character(1)
      )
      if (length(self$unresolved) > 0L) {
        stop(
          sprintf("Unresolved classIDs: %s", paste(self$unresolved, collapse = ", ")),
          call. = FALSE
        )
      }
      for (i in seq_along(entries)) {
        if (length(instances[[i]]) == 0L) next
        new_class <- resolved[[i]]$classObject()
        for (obj in instances[[i]])
          obj$resolveClassWith(new_class)
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
      if (!is.environment(envir))
        stop("`envir` must be an environment.", call. = FALSE)
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
