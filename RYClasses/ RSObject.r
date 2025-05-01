#' RSObject
RSObject <- R6::R6Class(
  ### class name ###
  "RSObject",
  cloneable = FALSE,
  public = list(
    initialize = function() {
      # get Class Object
      class_name <- class(self)[1]  ## Only here, get classname from func class().
      base_env   <- environment(self$initialize)
      search_env <- if (identical(base_env, .GlobalEnv)) parent.env(base_env) else base_env
      class_def  <- get(class_name, envir = search_env, inherits = TRUE)
      private$.__class__ <- class_def
      invisible(self)
    },
    class = function() {
      private$.__class__
    },
    conformsTo = function(protocol) {
      if (is.null(private$.__protocols__)) return(FALSE)
      protocol %in% private$.__protocols__
    },
    respondsTo = function(method) {
      in_public <-
        exists(method, where = self, inherits = TRUE) &&
        is.function(self[[method]])
      in_private <-
        exists(method, where = private, inherits = TRUE) &&
        is.function(self[[method]])
      in_public || in_private
    },
    description = function() {
      paste0("<", self$class()$classname, " ", private$.pointerHex(), ">")
    },
    debugDescription = function() {
      descript <- self$description()
      fields <- ls(self, all.names = TRUE)
      if (length(fields) == 0) return(paste0(descript, "{ }"))
      field_values <- vapply(fields,
        function(f){
          value <- tryCatch(self[[f]], error = function(e){"<error>"})
          val_str <- capture.output(str(value, max.level = 1))
          paste(val_str, collapse = " ")
        },
        character(1)
      )
      fields_info <- paste(fields, ": ", field_values, collapse =", ")
      paste0(descript, "{", fields_info, "}")
    },
    print = function() {
      cat(self$description(), "\n")
      invisible(self)
    },
    isMemberOf = function(cls) {
      my_cls <- self$class()
      identical(my_cls, cls)
    },
    isKindOf = function(cls) {
      any(
        vapply(
          private$.inheritClasses(),
          function(kls) identical(kls, cls),
          logical(1)
        )
      )
    },
    isEqual = function(other) {
      identical(self, other)
    },
    hash = function() {
      hex_str <- private$.pointerHex()
      padded <- sprintf("%016s", tolower(sub("^0x", "", hex_str)))
      unit3 <- substr(padded, 9, 12)
      unit4 <- substr(padded, 13, 16)
      u3 <- strtoi(unit3, base = 16L)
      u4 <- strtoi(unit4, base = 16L)
      return(bitwOr(bitwShiftL(u3, 16L), u4))
    }
  ),
  private = list(
    .__class__ = NULL,
    .__protocols__ = NULL,
    .pointerHex = function() {
      addr <- capture.output(environment(self$print))
      # addr <- capture.output(self$.__enclos_env__)
      hex_match <- sub("^<environment: (0x[0-9a-f]+)>$", "\\1", addr)
      if (is.na(hex_match)) {
        stop("RSObject internal pointer (.ptr) could not be extracted.")
      }
      return(hex_match)
    },
    .inheritClasses = function() {
      cls <- private$.__class__
      if (!is.null(cls$rs_inherit_chain)) return(cls$rs_inherit_chain)
      rs_inherit_chain <- list()
      while (!is.null(cls)) {
        size <- length(rs_inherit_chain)
        rs_inherit_chain[[size + 1]] <- cls
        if (!is.symbol(cls$inherit)) {
          cls <- cls$inherit
        } else {
          cls <- eval(cls$inherit)
        }
      }
      cls <- private$.__class__
      cls$rs_inherit_chain <- rs_inherit_chain
      lockBinding("rs_inherit_chain", cls)
      return(cls$rs_inherit_chain)
    }
  )
);
RSObject$.__class__ <- RSObject;
RSObject$rs_inherit_chain <- NULL;
RSObject$new();