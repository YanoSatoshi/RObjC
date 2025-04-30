#' ContractRev Definition
ClassContractRev <- R6::R6Class(
  ### class name ###
  "ContractRev",
  inherit   = ClassContractRoot,
  cloneable = FALSE,
  public = list(
    date_rev_start = as.Date(NA),
    date_rev_end   = as.Date(NA),
    date_fmc_end   = as.Date(NA),
    initialize = function(row) {
      super$initialize(row)
      self$date_rev_start <- row$ctrct_rev_pstart
      self$date_rev_end   <- row$ctrct_rev_pend
      self$date_fmc_end   <- row$ctrct_fmc_pend
      invisible(self)
    }
  ),
  private = list(
    ## methods required over-ride
    initRowConv = function() {
      set_names(
        c('year', 'ctrct_revID', 'ctrct_rev_name', 'ctrct_rev_inform_mail'),
        'year', 'ID', 'groupName', 'date_info_mail'
      )
    }
  )
)



# td_ctrct_rev <- tibble(
#   ctrct_revID = 10,
#   ctrct_rev_name = "ctrct_rev",
#   ctrct_rev_inform_mail = as.Date("1999-12-31"),
#   instID = 10,
#   deptID = 10,
#   status_inst = "paused",
#   status_dept = "stopped",
#   ##
#   ctrct_rev_pstart = as.Date("2000-02-01"),
#   ctrct_rev_pend = as.Date("2000-02-02"),
#   ctrct_fmc_pend = as.Date("2000-02-03")
# )