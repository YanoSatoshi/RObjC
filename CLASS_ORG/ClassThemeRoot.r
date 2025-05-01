#' ThemeOrg Definition
RSThemeRoot <- R6::R6Class(
  ### class name ###
  "RSThemeRoot",
  cloneable = FALSE,
  public = list(
    ID   = as.integer(NA),
    year = as.integer(NA),
    memberID = as.integer(NA),
    deptID   = as.integer(NA),
    instID   = as.integer(NA),
    info_mail = as.Date(NA),
    status   = str2fct_item_status(NA), # Utility.r
    contractID = as.integer(NA),
    # contract = NULL, # ClassContractOrg
    initialize = function(row) {
      conv <- private$initRowConv()
      self$ID   <- row[[1, conv['ID']]]
      self$year <- row[[1, conv['year']]]
      self$date_info_mail <- row[[1, conv['info_mail']]]
      self$status   <- str2fct_item_status(row[[1, conv['status']]])
      self$memberID <- row$memberID
      self$deptID   <- row$deptID
      self$instID   <- row$instID
      invisible(self)
    },
    setContract = function(contract) {

    },
    summary = function() {
      src_head_former <- c('year', 'memberID', 'deptID', 'instID,' 'contractID')
      src_head_later  <- c('info_mail', 'status')
      src_data_former <- c(self$year, self$memberID, self$deptID, self$instID, self$contractID)
      src_data_later  <- list(self$info_mail, self$status)
    }
  ),
  private = list(
    ## should over-ride
    initRowConv = function() {
      warning(msg_not_overridden(self, "initRowConv"))
    }
  )
)
