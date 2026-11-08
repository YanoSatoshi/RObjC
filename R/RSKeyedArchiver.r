#' @include RSCoder.r runtime_class_registry.r
#' @title RSKeyedArchiver: Encode Logical Object State
#' @description
#' Creates an archive containing class identifiers, keyed logical state, and
#' reference IDs made from source object pointers. Original RObjC objects are
#' retained throughout an archive operation, so their addresses cannot be
#' reused mid-encode. On decode these strings are opaque reference IDs, not
#' addresses of restored objects.
#' Lists are traversed to preserve references inside collection fields; other
#' base R values form the R serialization boundary.
#' @section Protocols:
#' - [RSCopying]
#'
#' @examples
#' # Archive one root object.
#' original <- RSString$new("saved text")
#' archive <- RSKeyedArchiver$new()$encode(original)
#' stopifnot(is.list(archive), length(archive$records) == 1L)
#' # saveRDS(archive, save_to_path)
#'
#' # A list supplies multiple named roots. Shared objects are recorded once.
#' shared <- RSString$new("shared")
#' roots <- list(first = shared, second = RSNumber$new(2L), again = shared)
#' multiple <- RSKeyedArchiver$new()$encode(roots)
#' stopifnot(length(multiple$records) == 2L)
#' # saveRDS(multiple, save_to_path)
#'
#' @export
RSKeyedArchiver <- RSObjectClass(
  "RSKeyedArchiver",
  version = "0.2.0",
  super_class = RSCoder,
  private = list(
    .retained = NULL,
    .records = NULL,
    .current_fields = NULL,

    .encode_value = function(value) {
      if (robjc_isRObjC(value)) {
        address <- robjc_get_pointer(value)
        # check `.retained` cache
        if (exists(address, envir = private$.retained, inherits = FALSE))
          return(list(type = "ref", id = address))

        # new archiving
        private$.retained[[address]] <- value
        if (robjc_isInstance(value)) {
          # instance
          generator <- value$class()
          kind      <- "instance"
        } else {
          # class or protocol
          generator <- value
          kind      <- if (robjc_isClass(value)) "class" else "protocol"
        }
        # get stable ID for the generator
        identity <- .robjc_registered_identity(generator)
        # make record template & store it
        record <- list(
          kind        = kind,
          class_name  = generator$classname,
          version     = generator$robjc_isa$version,
          package     = identity$package,
          class_id    = identity$id,
          fields      = NULL
        )
        private$.records[[address]] <- record

        # stores the instance data
        if (identical(kind, "instance")) {
          if (!value$respondsTo("encodeWithCoder"))
            stop(
              sprintf("%s does not implement encodeWithCoder().", generator$classname),
              call. = FALSE
            )
          # leave current storage, set new storage for encoding instance
          fields   <- new.env(hash = FALSE, parent = emptyenv())
          previous <- private$.current_fields
          private$.current_fields <- fields
          tryCatch(
            value$encodeWithCoder(self),
            finally = { private$.current_fields <- previous } # restore the prev
          )
          record$fields <- as.list(fields, all.names = TRUE)
          private$.records[[address]] <- record
        }
        return(list(type = "ref", id = address))
      }
      if (is.list(value) && !is.object(value))
        return(list(type = "list", values = lapply(value, private$.encode_value)))
      list(type = "r", value = value)
    }
  ),
  public = list(
    #' @description Initializes an empty coder.
    initialize = function() {
      super$initialize()
      private$.retained <- new.env(hash = TRUE, parent = emptyenv())
      private$.records  <- new.env(hash = TRUE, parent = emptyenv())
      invisible(self)
    },



    #' @description Records a field of the object currently being encoded.
    #' @param object Value to encode.
    #' @param key One nonempty character string.
    #' @return `self`, invisibly.
    encodeObjectForKey = function(object, key) {
      .robjc_coder_check_key(key)
      if (is.null(private$.current_fields))
        stop("No object is currently being encoded.", call. = FALSE)
      if (exists(key, envir = private$.current_fields, inherits = FALSE))
        stop(sprintf("Duplicate coding key: %s", key), call. = FALSE)
      private$.current_fields[[key]] <- private$.encode_value(object)
      invisible(self)
    },



    #' @description Encodes a root value as a portable record graph.
    #' @param object Root RObjC object or base R value.
    #' @return A plain R list suitable for `saveRDS()`.
    encode = function(object) {
      private$.retained <- new.env(hash = TRUE,  parent = emptyenv())
      private$.records  <- new.env(hash = FALSE, parent = emptyenv())
      private$.current_fields <- NULL
      on.exit(
        {
          private$.retained <- new.env(hash = TRUE,  parent = emptyenv())
          private$.records  <- new.env(hash = FALSE, parent = emptyenv())
          private$.current_fields <- NULL
        },
        add = TRUE
      )
      root <- private$.encode_value(object)
      list(
        format = "robjc-keyed-1",
        root = root,
        records = as.list(private$.records, all.names = TRUE)
      )
    }
  )
)
