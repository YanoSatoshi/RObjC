#' OrgOrganization Definition
RSOrgRoot <- R6::R6Class(
  ### class name ###
  "RSOrgRoot",
  cloneable = FALSE,
  inherit = ClassRoot,
  public = list(
    year = as.integer(NA),
    ID = as.integer(NA),
    name_full  = as.character(NA),
    name_short = as.character(NA),
    status = str2fct_org_status(NA),
    initialize = function(row) {
      super$initialize()
      conv <- private$initRowConv()
      self$year <- row[[1, conv['year']]]
      self$ID   <- row[[1, conv['ID']]]
      self$name_full  <- row[[1, conv['name_full']]]
      self$name_short <- row[[1, conv['name_short']]]
      invisible(self)
    }
  ),
  private = list(
    ## should over-ride
    initRowConv = function() {
      warning(msg_not_overridden(self, "initRowConv"))
    }
  )
)
