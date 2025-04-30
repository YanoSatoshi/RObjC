#' ContractItv Definition
ClassContractItv <- R6::R6Class(
  ### class name ###
  "ContractItv",
  inherit   = ClassContractRoot,
  cloneable = FALSE,
  public = list(
    date_rsv_start = as.Date(NA),
    date_rsv_end   = as.Date(NA),
    date_exc_start = as.Date(NA),
    date_exc_end   = as.Date(NA),
    initialize = function(row) {
      super$initialize(row)
      self$date_rsv_start <- row$ctrct_itv_rsv_pstart
      self$date_rsv_end   <- row$ctrct_itv_rsv_pend
      self$date_exc_start <- row$ctrct_itv_exc_pstart
      self$date_exc_end   <- row$ctrct_itv_exc_pend
      invisible(self)
    }
  ),
  private = list(
    ## methods required over-ride
    initRowConv = function() {
      set_names(
        c('year', 'ctrct_itvID', 'ctrct_itv_name', 'ctrct_itv_inform_mail'),
        'year', 'ID', 'groupName', 'date_info_mail'
      )
    }
  )
)



# td_ctrct_itv <- tibble(
#   ctrct_itvID = 0,
#   ctrct_itv_name = "ctrct_itv",
#   ctrct_itv_inform_mail = as.Date("1999-12-31"),
#   instID = 0,
#   deptID = 0,
#   status_inst = "available",
#   status_dept = "paused",
#   ##
#   ctrct_itv_rsv_pstart = as.Date("2000-01-01"),
#   ctrct_itv_rsv_pend = as.Date("2000-01-02"),
#   ctrct_itv_exc_pstart = as.Date("2000-01-03"),
#   ctrct_itv_exc_pend = as.Date("2000-01-04")
# )