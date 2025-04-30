#' Scalar
RSScalar <- R6::R6Class(
  ### class name ###
  "RSScalar",
  cloneable = FALSE,
  inherit = RSObject,
  public = list(
    initialize = function(val) {
      super$initialize()
      ## CHECK LENGTH ARG.
      ## IF NOT 1, THEN
      if (length(val) != 1L) {
        stop(
          sprintf(
            "%s can only accept length 1L vector.",
            self$class()$classname
          )
        )
      }
      stopifnot(!is.null(self$class()$isNAClass))
      # IN INHERITING, CLASS DO AS FOLLOWS
      #   SomeClass <- R6::R6Class(...)
      #   SomeClass$isNAClass <- TRUE/FALSE  ... TRUE on RS*** class, FALSE on RSNA*** class
      #   # additonary option
      #   SomeClass$***With <- function(val) {
      #     if (is.na(val)) return(RSNA***$new())
      #     return(RS***$new(val))
      #   }
      if (is.na(val) && !self$class()$isNAClass) {
        stop(
          sprintf("use %s for NA.", sub("RS", "RSNA", self$class()$classname))
        )
      }
      ## SET VAL
      private$.val <- val
      private$.hash_operator <- RSHashOperator$module()
      invisible(self)
      ## IN INHERITING CLASS, DO AS FOLLOWS
      # initialize = functin(val) {
      #   if (!inherits(val, 'some R class name')) stop("val must be of class some_class_name")
      #   super$initialize(val)
      #   invisible(self)
      # }
      ## END
    },

    compare = function(other) {
      if (!self$isMemberOf(other$class())) {
        stop("Cannot compare objects belonging to different classes.")
      }
      val1 <- self$value()
      val2 <- other$value()
      if (is.na(val1) || is.na(val2)) {
        return(NA_integer_)
      }
      if (val1 < val2) {
        return(-1L)
      } else if (val1 > val2) {
        return(1L)
      } else {
        return(0L)
      }
    },

    description = function() {
      sprintf(
        "<%s %s> %s",
        self$class()$classname, private$.pointerHex(),
        as.character(private$.val)
      )
    },

    isEqual = function(other) {
      isTRUE(self$isMemberOf(other$class()) && private$.val == other$value())
    },

    hash = function() {
      private$.hash_operator$hashForScalar(private$.val)
    },

    value = function() {
      private$.val
    }
  ),

  private = list(
    .val = NULL,
    .hash_operator = NULL
  )
)
RSScalar$isNAClass = NULL



RSScalar$valueWith <- function(val) {
  if (is.null(val)) {
    return(RSNull$null())
  }
  if (length(val) != 1) {
    stop(sprintf(
      "Only scalar (length 1) values can be wrapped. Provided length: %d",
      length(val)
    ))
  }
  cls <- class(val)[1]
  if (cls == "numeric" || cls == "integer") {
    return(RSNumber$numberWith(val))
  } else if (cls == "character") {
    return(RSString$stringWith(val))
  } else if (cls == "factor") {
    return(RSFactor$factorWith(val))
  } else if (cls == "logical") {
    return(RSBoolean$boolWith(val))
  } else if (cls == "POSIXct") {
    return(RSDateTime$dateTimeWith(val))
  } else if (cls == "Date") {
    return(RSDate$dateWith(val))
  } else {
    stop(sprintf("Unsupported scalar type: %s", cls))
  }
}
