#' RSThemeItv Definition
RSThemeItv <- R6::R6Class(
  ### class name ###
  "RSThemeItv",
  inherit   = ClassThemeRoot,
  cloneable = FALSE,
  public = list(
    initialize = function(row) {
      super$initialize(row)
      private$rsv_start <- row$itv_rsv_pstart
      private$rsv_end   <- row$itv_rsv_pend
      private$exc_start <- row$itv_exc_pstart
      private$exc_end   <- row$itv_exc_pend
      invisible(self)
    },
    hasOwnRsvPeriod   = function() {
      !is.na(c(private$rsv_start, private$rsv_end)) %>% all()
    },
    getReservePeriod  = function() {
      if (self$hasOwnRsvPeriod()) {
        c(rsv_start = private$rsv_start, rsv_end = private$rsv_end)
      } else {

      }
    },
    hasOwnExcPeriod   = function() {
      !is.na(c(private$rsv_start, private$rsv_end)) %>% all()
    },
    getExcutionPeriod = function() {
      if (self$hasOwnExcPeriod()) {
        c(exc_start = private$exc_start, exc_end = private$exc_end)
      } else {
        
      }
    }
  ),
  private = list(
    rsv_start = as.Date(NA),
    rsv_end   = as.Date(NA),
    exc_start = as.Date(NA),
    exc_end   = as.Date(NA),
    initRowConv = function() {
      set_names(
        c('year', 'itv_themeID', 'interview_inform_mail', 'interview_status'),
        'year', 'ID', 'date_info_mail', 'status'
      )
    }
  )
)
