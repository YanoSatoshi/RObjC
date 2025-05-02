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
      #   SomeClass <- R6::R6Class(...)  # nolint
      #   SomeClass$isNAClass <- TRUE/FALSE  ... TRUE on RS*** class, FALSE on RSNA*** class  # nolint
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
      #   if (!inherits(val, 'some R class name')) stop("val must be of class some_class_name") # nolint
      #   super$initialize(val)  # nolint
      #   invisible(self)  # nolint
      # }
      ## END
    },

    compare = function(other, na.last = TRUE) {
      if (!private$.comparable(other)) {
        stop(sprintf(
          "Abort trying to compare %s and %s",
          self$class()$classname, other$class()$classname
        ))
      }
      na_val <- if (na.last) .Machine$double.xmax else .Machine$double.xmin
      vec  <- xtfrm(c(self$value(), other$value()))
      val1 <- if (is.na(vec[1])) na_val else vec[1]
      val2 <- if (is.na(vec[2])) na_val else vec[2]
      return(sign(val1 - val2))
    },

    description = function() {
      sprintf(
        "<%s %s> %s",
        self$class()$classname, private$.pointerHex(),
        as.character(private$.val)
      )
    },

    isEqual = function(other) {
      self$compare(other) == 0L
    },

    hash = function() {
      private$.hash_operator$hashForScalar(private$.val)
    },

    value = function() {
      private$.val
    },

    copyWithEnv = function(env) {
      if (identical(env, environment(self))) return(self)
      klass <- self$class()
      if (!klass$isNAClass) {
        do.call(klass$new, list(private$.val), envir = env)
      } else {
        do.call(klass$new, envir = env)
      }
    }

  ),

  private = list(
    .val = NULL,
    .hash_operator = NULL,
    .comparable = function(other) {
      if (xor(self$class()$isNAClass, other$class()$isNAClass)) {
        # self or other is kind of NAClass
        if (length(class(self)) < length(class(other))) {
          return(other$isKindOf(self$class()))
        } else {
          return(self$isKindOf(other$class()))
        }
      } else {
        # both self and other are NAClasses or Not-NAClasses
        return(self$isMemberOf(other$class()))
      }
    }
  )
)
RSScalar$isNAClass = NULL  # nolint



RSScalar$valueWith <- function(val) {  # nolint
  if (is.null(val)) {
    return(RSNull$null()) # nolint
  }
  if (length(val) != 1) {
    stop(sprintf(
      "Only scalar (length 1) values can be wrapped. Provided length: %d",
      length(val)
    ))
  }
  cls <- class(val)[1]
  if (cls == "numeric" || cls == "integer") {
    return(RSNumber$numberWith(val))  # nolint
  } else if (cls == "character") {
    return(RSString$stringWith(val))  # nolint
  } else if (cls == "factor") {
    return(RSFactor$factorWith(val))  # nolint
  } else if (cls == "logical") {
    return(RSBoolean$boolWith(val))  # nolint
  } else if (cls == "POSIXct") {
    return(RSDateTime$dateTimeWith(val))  # nolint
  } else if (cls == "Date") {
    return(RSDate$dateWith(val))  # nolint
  } else {
    stop(sprintf("Unsupported scalar type: %s", cls))
  }
}
