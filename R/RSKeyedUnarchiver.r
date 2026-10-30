#' @include RSCoder.r runtime_class_registry.r
#' @title RSKeyedUnarchiver: Restore Logical Object State
#' @description
#' Loads a keyed archive for inspection and class mapping, then restores it
#' when `$decode()` is called. `$showAllClasses()` returns classes needing
#' attention by default, and `$setClasses()` accepts an edited tibble or
#' [RSMutableDictionary].
#' A caller-supplied resolver remains available for older archives and
#' classes defined outside a package namespace.
#'
#' The default instance factory uses [robjc_allocate()] to create an instance
#' without calling `initialize()`, then invokes its `initWithCoder()` method.
#' @export
RSKeyedUnarchiver <- RSObjectClass(
  "RSKeyedUnarchiver",
  version = "0.1.0 beta",
  super_class = RSCoder,
  private = list(
    .archive = NULL,
    .resolve_class = NULL,
    .make_instance = NULL,
    .object_map = NULL,
    .current_fields = NULL,
    .current_record = NULL,
    .class_table = NULL,
    .record_keys = NULL,

    .prepare_classes = function() {
      if (!is.null(private$.class_table)) return(invisible(NULL))
      records <- private$.archive$records
      record_keys <- new.env(hash = TRUE, parent = emptyenv())
      signatures <- list()
      rows <- list()
      for (record_id in names(records)) {
        record <- records[[record_id]]
        if (!is.list(record) || !is.character(record$kind) ||
            length(record$kind) != 1L || is.na(record$kind) ||
            !record$kind %in% c("instance", "class", "protocol") ||
            !is.character(record$class_name) || length(record$class_name) != 1L ||
            is.na(record$class_name) || !nzchar(record$class_name) ||
            !is.character(record$version) || length(record$version) != 1L ||
            is.na(record$version) || !nzchar(record$version) ||
            xor(is.null(record$package), is.null(record$class_id)) ||
            (!is.null(record$package) &&
             (!is.character(record$package) || length(record$package) != 1L ||
              is.na(record$package) || !nzchar(record$package) ||
              !is.character(record$class_id) || length(record$class_id) != 1L ||
              is.na(record$class_id) || !nzchar(record$class_id))))
          stop("Invalid archive class record.", call. = FALSE)
        kind <- if (identical(record$kind, "protocol")) "protocol" else "class"
        signature <- list(
          package = record$package, class_id = record$class_id,
          class_name = record$class_name, version = record$version,
          kind = kind
        )
        matching <- which(vapply(signatures, identical, logical(1), signature))
        if (length(matching)) {
          index <- matching[[1L]]
        } else {
          index <- length(rows) + 1L
          signatures[[index]] <- signature
          candidate <- if (!is.null(private$.resolve_class)) {
            private$.resolve_class(record$class_name, record$version, record$kind)
          } else if (!is.null(record$package) && !is.null(record$class_id)) {
            tryCatch(
              .robjc_resolve_registered(record$package, record$class_id, record$kind),
              error = function(e) NULL
            )
          } else NULL
          valid <- if (identical(kind, "protocol"))
            robjc_isProtocol(candidate) else robjc_isClass(candidate)
          if (!valid && !is.null(candidate) && !is.null(private$.resolve_class))
            stop(sprintf("Resolver returned an invalid generator for %s.",
                         record$class_name), call. = FALSE)
          if (!valid) candidate <- NULL
          rows[[index]] <- list(
            class_key = paste0("C", index),
            package = if (is.null(record$package)) NA_character_ else record$package,
            class_id = if (is.null(record$class_id)) NA_character_ else record$class_id,
            class_name = record$class_name,
            version = if (is.null(record$version)) NA_character_ else record$version,
            kind = kind,
            decoder_class = list(candidate),
            decoder_name = if (is.null(candidate)) NA_character_ else candidate$classname,
            status = if (is.null(candidate)) "unresolved" else
              if (is.null(private$.resolve_class)) "automatic" else "resolver"
          )
        }
        record_keys[[record_id]] <- rows[[index]]$class_key
      }
      if (length(rows)) {
        strings <- function(name) vapply(rows, function(row) row[[name]], character(1))
        private$.class_table <- tibble::tibble(
          class_key = strings("class_key"), package = strings("package"),
          class_id = strings("class_id"), class_name = strings("class_name"),
          version = strings("version"), kind = strings("kind"),
          decoder_class = lapply(rows, function(row) row$decoder_class[[1L]]),
          decoder_name = strings("decoder_name"), status = strings("status")
        )
      } else {
        private$.class_table <- tibble::tibble(
          class_key = character(), package = character(), class_id = character(),
          class_name = character(), version = character(), kind = character(),
          decoder_class = list(), decoder_name = character(), status = character()
        )
      }
      private$.record_keys <- record_keys
      invisible(NULL)
    },

    .decode_value = function(node) {
      if (!is.list(node) || !is.character(node$type) || length(node$type) != 1L)
        stop("Invalid archive node.", call. = FALSE)
      if (identical(node$type, "r"))
        return(node$value)
      if (identical(node$type, "list"))
        return(lapply(node$values, private$.decode_value))
      if (!identical(node$type, "ref"))
        stop("Unknown archive node type.", call. = FALSE)

      id <- node$id
      if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id))
        stop("Invalid archive reference ID.", call. = FALSE)
      if (exists(id, envir = private$.object_map, inherits = FALSE))
        return(private$.object_map[[id]])
      record <- private$.archive$records[[id]]
      if (is.null(record))
        stop(sprintf("Missing archive record: %s", id), call. = FALSE)
      key <- private$.record_keys[[id]]
      index <- match(key, private$.class_table$class_key)
      generator <- private$.class_table$decoder_class[[index]]
      if (identical(record$kind, "class") || identical(record$kind, "protocol")) {
        expected <-
          if (identical(record$kind, "class")) robjc_isClass(generator) else robjc_isProtocol(generator)
        if (!expected) stop("Resolver returned an invalid generator.", call. = FALSE)
        private$.object_map[[id]] <- generator
        return(generator)
      }
      if (!identical(record$kind, "instance") || !robjc_isClass(generator))
        stop("Resolver returned an invalid object class.", call. = FALSE)
      object <- private$.make_instance(generator)
      if (!robjc_isRobjcObject(object) || !object$isKindOf(generator))
        stop("Instance factory returned an invalid object.", call. = FALSE)
      if (!object$respondsTo("initWithCoder"))
        stop("Decoded class does not implement initWithCoder().", call. = FALSE)
      private$.object_map[[id]] <- object
      previous <- private$.current_fields
      previous_record <- private$.current_record
      private$.current_fields <- record$fields
      private$.current_record <- record
      tryCatch(
        object$initWithCoder(self),
        finally = {
          private$.current_fields <- previous
          private$.current_record <- previous_record
        }
      )
      object
    }
  ),
  public = list(
    #' @description Prepares a keyed archive for decoding.
    #' @param archive Archive created by [RSKeyedArchiver]`$archive()`.
    #' @param resolve_class Optional function receiving class name, saved
    #'   version, and kind (`"instance"`, `"class"`, or `"protocol"`). If
    #'   supplied, it takes precedence over package registration.
    #' @param make_instance Optional function receiving a current generator and
    #'   returning an instance. Defaults to [robjc_allocate()].
    initialize = function(
      archive,
      resolve_class = NULL,
      make_instance = robjc_allocate
    ) {
      super$initialize()
      if (!is.list(archive) || !identical(archive$format, "robjc-keyed-1") ||
          !is.list(archive$records) || is.null(archive$root))
        stop("Invalid or unsupported keyed archive.", call. = FALSE)
      if (length(archive$records) &&
          (is.null(names(archive$records)) || anyNA(names(archive$records)) ||
           any(!nzchar(names(archive$records))) || anyDuplicated(names(archive$records))))
        stop("Archive records need unique reference IDs.", call. = FALSE)
      if ((!is.null(resolve_class) && !is.function(resolve_class)) ||
          !is.function(make_instance))
        stop("Resolver and instance factory must be functions.", call. = FALSE)
      private$.archive        <- archive
      private$.resolve_class  <- resolve_class
      private$.make_instance  <- make_instance
      private$.object_map     <- new.env(hash = TRUE, parent = emptyenv())
      cat("Archive loaded. Use $showAllClasses() or $dictionaryWithAllClasses(), then $setClasses(mapping) and $decode().\n")
      invisible(self)
    },

    #' @description Shows archived classes and their proposed decoder classes.
    #'   By default, classes resolved automatically from package registration
    #'   are omitted. Explicit resolver results and manual mappings remain
    #'   visible. Use `remove_default = FALSE` to show every class.
    #' @param remove_default Whether to omit automatically resolved classes.
    #' @return A tibble with a `decoder_class` list column of generators or `NULL`.
    showAllClasses = function(remove_default = TRUE) {
      if (!is.logical(remove_default) || length(remove_default) != 1L ||
          is.na(remove_default))
        stop("`remove_default` must be TRUE or FALSE.", call. = FALSE)
      private$.prepare_classes()
      if (remove_default)
        private$.class_table[private$.class_table$status != "automatic", , drop = FALSE]
      else
        private$.class_table
    },

    #' @description Returns all current class mappings as a mutable dictionary.
    #'   Keys are `RSString` objects containing the `class_key` values shown by
    #'   `$showAllClasses(remove_default = FALSE)`. Values are decoder generators; unresolved classes
    #'   have [RSNull]`$null` as a placeholder. The returned dictionary is an
    #'   independent snapshot. Pass it to `$setClasses()` after editing it.
    #' @return An [RSMutableDictionary].
    dictionaryWithAllClasses = function() {
      private$.prepare_classes()
      dictionary <- RSMutableDictionary$new()
      for (row in seq_len(nrow(private$.class_table))) {
        generator <- private$.class_table$decoder_class[[row]]
        if (is.null(generator)) generator <- RSNull$null
        dictionary$setObjectForKey(
          generator,
          RSString$new(private$.class_table$class_key[[row]])
        )
      }
      dictionary
    },

    #' @description Shows one class mapping by the `class_key` shown in
    #'   `$showAllClasses(remove_default = FALSE)`.
    #' @param class_key One class key, such as `"C1"`.
    #' @return A one-row tibble.
    showClass = function(class_key) {
      private$.prepare_classes()
      if (!is.character(class_key) || length(class_key) != 1L || is.na(class_key))
        stop("`class_key` must be one string.", call. = FALSE)
      index <- match(class_key, private$.class_table$class_key)
      if (is.na(index)) stop("Unknown archive class key.", call. = FALSE)
      private$.class_table[index, , drop = FALSE]
    },

    #' @description Registers decoder classes from an edited tibble or an
    #'   [RSMutableDictionary] whose `RSString` keys are archive class keys and
    #'   whose values are class or protocol generators. Use [RSNull]`$null` in
    #'   a dictionary for an unresolved class. Missing rows or entries leave
    #'   their current mappings unchanged.
    #' @param mapping A tibble with `class_key` and `decoder_class` columns, or
    #'   an `RSMutableDictionary`.
    #' @return `self`, invisibly.
    setClasses = function(mapping) {
      private$.prepare_classes()
      if (robjc_isRobjcObject(mapping) && mapping$isKindOf(RSMutableDictionary)) {
        entries <- mapping$allEntriesAsList()
        keys <- vapply(entries, function(entry) {
          key <- entry$key()
          if (!robjc_isRobjcObject(key) || !key$isKindOf(RSString))
            stop("Dictionary keys must be RSString class keys.", call. = FALSE)
          key$value()
        }, character(1))
        classes <- lapply(entries, function(entry) {
          value <- entry$value()
          if (robjc_isRobjcObject(value) && value$isKindOf(RSNull)) NULL else value
        })
      } else if (inherits(mapping, "tbl_df")) {
        if (!all(c("class_key", "decoder_class") %in% names(mapping)))
          stop("Tibble needs `class_key` and `decoder_class` columns.", call. = FALSE)
        keys <- mapping$class_key
        classes <- mapping$decoder_class
      } else {
        stop("`mapping` must be a tibble or RSMutableDictionary.", call. = FALSE)
      }
      if (!is.character(keys) || anyNA(keys) || anyDuplicated(keys) ||
          any(!keys %in% private$.class_table$class_key) ||
          !is.list(classes) || length(classes) != length(keys))
        stop("Invalid or duplicate archive class keys.", call. = FALSE)
      for (i in seq_along(keys)) {
        row <- match(keys[[i]], private$.class_table$class_key)
        generator <- classes[[i]]
        valid <- is.null(generator) ||
          if (identical(private$.class_table$kind[[row]], "protocol"))
            robjc_isProtocol(generator) else robjc_isClass(generator)
        if (!valid)
          stop(sprintf("Invalid decoder generator for %s.", keys[[i]]), call. = FALSE)
      }
      updated <- private$.class_table
      for (i in seq_along(keys)) {
        row <- match(keys[[i]], updated$class_key)
        generator <- classes[[i]]
        if (identical(generator, updated$decoder_class[[row]])) next
        updated$decoder_class[row] <- list(generator)
        updated$decoder_name[[row]] <- if (is.null(generator)) NA_character_ else generator$classname
        updated$status[[row]] <- if (is.null(generator)) "unresolved" else "manual"
      }
      private$.class_table <- updated
      invisible(self)
    },

    #' @description Reads a field of the object currently being decoded.
    #' @param key One nonempty character string.
    #' @return The decoded value, or `NULL` for an absent key.
    decodeObjectForKey = function(key) {
      .robjc_coder_check_key(key)
      if (is.null(private$.current_fields))
        stop("No object is currently being decoded.", call. = FALSE)
      if (!key %in% names(private$.current_fields))
        return(NULL)
      private$.decode_value(private$.current_fields[[key]])
    },

    #' @description Returns the saved class version during `initWithCoder()`.
    #' @return Saved version string, or `NULL` outside decoding.
    savedClassVersion = function() {
      if (is.null(private$.current_record)) return(NULL)
      private$.current_record$version
    },

    #' @description Restores the archive root value.
    #' @return The restored object or base R value.
    decode = function() {
      private$.prepare_classes()
      automatic <- which(private$.class_table$status == "automatic")
      for (row in automatic) {
        package <- private$.class_table$package[[row]]
        id <- private$.class_table$class_id[[row]]
        kind <- private$.class_table$kind[[row]]
        if (is.na(package) || is.na(id)) next
        current <- tryCatch(.robjc_resolve_registered(package, id, kind),
                            error = function(e) NULL)
        if (is.null(current) ||
            (identical(kind, "protocol") && !robjc_isProtocol(current)) ||
            (identical(kind, "class") && !robjc_isClass(current))) {
          private$.class_table$decoder_class[row] <- list(NULL)
          private$.class_table$decoder_name[[row]] <- NA_character_
          private$.class_table$status[[row]] <- "unresolved"
        } else {
          private$.class_table$decoder_class[row] <- list(current)
          private$.class_table$decoder_name[[row]] <- current$classname
        }
      }
      unresolved <- vapply(private$.class_table$decoder_class, is.null, logical(1))
      if (any(unresolved))
        stop(sprintf("Unresolved archive classes: %s",
                     paste(private$.class_table$class_key[unresolved], collapse = ", ")),
             call. = FALSE)
      for (record_id in names(private$.archive$records)) {
        record <- private$.archive$records[[record_id]]
        if (!identical(record$kind, "instance")) next
        key <- private$.record_keys[[record_id]]
        row <- match(key, private$.class_table$class_key)
        generator <- private$.class_table$decoder_class[[row]]
        supports_init <- any(vapply(
          robjc_inheritChain(generator$robjc_isa),
          function(klass) "initWithCoder" %in% names(klass$public_methods),
          logical(1)
        ))
        if (!supports_init)
          stop(sprintf("Decoded class %s does not implement initWithCoder().",
                       generator$classname), call. = FALSE)
      }
      private$.object_map <- new.env(hash = TRUE, parent = emptyenv())
      private$.current_fields <- NULL
      private$.current_record <- NULL
      private$.decode_value(private$.archive$root)
    },

    #' @description Compatibility alias for `$decode()`.
    #' @return The restored archive root.
    unarchive = function() {
      self$decode()
    }
  )
)
