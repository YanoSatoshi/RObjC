#' @include runtime_robjc.r


#' @title Format an RObjC Instance
#'
#' @description Formats an RObjC instance using its generator's `$classname`.
#'  Public and private data members are shown. Methods are included when
#'  `method = TRUE`. The `description()` and `debugDescription()` methods
#'  return the same text for the corresponding `method` setting.
#' @param x An RObjC instance.
#' @param method Whether to include methods among the displayed members.
#'   Defaults to `FALSE`.
#' @param ... Additional arguments passed by `print()` to `format()`.
#' @return A character string containing the formatted instance.
#' @rdname format.RSObject
#' @examples
#' obj <- RSString$new("hello")
#' print(obj)
#' # <RSString> RObjC instance
#' #   Inherits from: <RSScalar>
#' format(obj)                # Includes private data members.
#' format(obj, method = TRUE) # Also includes methods.
#' @export
format.RSObject <- function(x, method = FALSE, ...) {
  klass <- x$class()
  lines <- c(paste0("<", klass$classname, "> RObjC instance"))
  if (!is.null(klass$robjc_isa$super))
    lines <- c(
      lines,
      paste0("  Inherits from: <", klass$robjc_isa$super$classname, ">")
    )
  lines <- c(
    lines,
    "  Public:",
    robjc_member_summaries(
      x,
      exclude = c(".__active__", ".__enclos_env__"),
      include_methods = method
    )
  )
  private_members <- x$.__enclos_env__$private
  private_lines <- robjc_member_summaries(
    private_members,
    exclude = ".__isa",
    include_methods = method
  )
  if (length(private_lines) > 0L)
    lines <- c(lines, "  Private:", private_lines)
  paste(lines, collapse = "\n")
}



#' @rdname format.RSObject
#' @export
print.RSObject <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}

robjc_isa_value_summary <- function(value) {
  if (is.null(value))
    return(
      "NULL"
    )
  if (robjc_isMetaClass(value))
    return(paste0(
      "<", value$classname, ">"
    ))
  if (robjc_isInstance(value))
    return(paste0(
      "<", value$.__enclos_env__$private$.__isa$class$classname, " instance>"
    ))
  if (is.list(value)) {
    if (length(value) == 0L)
      return(
        "list()"
      )
    items <- vapply(
      value,
      function(item) {
        if (robjc_isMetaClass(item))
          paste0("<", item$classname, ">")
        else if (is.null(item))
          "NULL"
        else
          typeof(item)
      },
      character(1))
    return(paste0(
      "list(", paste(items, collapse = ", "), ")"
    ))
  }
  if (is.atomic(value)) {
    if (length(value) == 0L)
      return(
        paste0(typeof(value), "(0)")
      )
    return(
      paste(as.character(value), collapse = ", ")
    )
  }
  if (is.environment(value))
    return("<environment>")
  return(typeof(value))
}



#' Format RObjC Class Identity
#'
#' @description Formats the identity record associated with an RObjC object
#'   class or protocol generator. Every field in the identity record is shown.
#'   Class names come from generator `$classname`.
#' @param x An RObjC identity record.
#' @param ... Unused.
#' @return A character string containing the formatted identity.
#' @examples
#' print(RSString$robjc_isa)
#' # <RSString> RObjC object identity
#' #   class: <RSString>
#' #   protocols: list()
#' #   slot: NULL
#' #   super: <RSScalar>
#' #   version: 0.1.0 beta
#' @export
format.RSIsA <- function(x, ...) {
  generator <- x$class
  kind <- if (inherits(x, ISA_PROTOCOL)) "protocol" else "object"
  lines <- c(paste0(
    "<", generator$classname, "> RObjC ", kind, " identity"
  ))
  field_names <- ls(x, all.names = TRUE)
  lines <- c(
    lines,
    vapply(
      field_names,
      function(name) paste0("  ", name, ": ", robjc_isa_value_summary(x[[name]])),
      character(1), USE.NAMES = FALSE
    )
  )
  paste(lines, collapse = "\n")
}



#' @rdname format.RSIsA
#' @export
print.RSIsA <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}



#' Format an RObjC Class Generator
#'
#' @description Formats an RObjC class or protocol generator, following the
#'   layout of R6's generator formatter.
#' @param x An RObjC class or protocol generator.
#' @param ... Unused.
#' @return A character string containing the formatted generator.
#' @examples
#' print(RSString)
#' # <RSString> RObjC object generator
#' #   Inherits from: <RSScalar>
#' #   Version: 0.1.0 beta
#' #   Public:
#' #     initialize: function (val)
#' #   Parent env: <environment: namespace:robjc>
#' #   Locked objects: TRUE
#' #   Locked class: FALSE
#' #   Portable: TRUE
#' @export
format.RobjcClass <- function(x, ...) {
  kind <- if (inherits(x$robjc_isa, ISA_PROTOCOL)) "protocol" else "object"
  lines <- c(
    paste0("<", x$classname, "> RObjC ", kind, " generator")
  )
  if (!is.null(x$robjc_isa$super))
    lines <- c(
      lines,
      paste0("  Inherits from: <", x$robjc_isa$super$classname, ">")
    )
  lines <- c(
    lines,
    paste0("  Version: ", x$robjc_isa$version)
  )
  lines <- c(
    lines,
    "  Public:",
    robjc_member_summaries(x$public_fields),
    robjc_member_summaries(x$public_methods)
  )
  if (!is.null(x$active))
    lines <- c(
      lines,
      "  Active bindings:", robjc_member_summaries(x$active)
    )
  private_fields <- robjc_member_summaries(x$private_fields, exclude = ".__isa")
  private_methods <- robjc_member_summaries(x$private_methods)
  if (length(private_fields) + length(private_methods) > 0L)
    lines <- c(
      lines,
      "  Private:", private_fields, private_methods
    )
  lines <- c(
    lines,
    paste("  Parent env:", format(x$parent_env))
  )
  if (!is.null(x$lock) && is.logical(x$lock))
    lines <- c(
      lines,
      paste("  Locked objects:", x$lock)
    )
  if (!is.null(x$lock_objects))
    lines <- c(
      lines,
      paste("  Locked objects:", x$lock_objects)
    )
  if (!is.null(x$lock_class))
    lines <- c(
      lines,
      paste("  Locked class:", x$lock_class)
    )
  lines <- c(
    lines,
    paste("  Portable:", x$portable)
  )
  paste(lines, collapse = "\n")
}



#' @rdname format.RobjcClass
#' @export
print.RobjcClass <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}
