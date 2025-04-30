#' ContractOrg Definition
RSContractRoot <- R6::R6Class(
  ### class name ###
  "RSContractRoot",
  cloneable = FALSE,
  public = list(
    year = as.integer(NA),
    ID = as.integer(NA),
    instID = as.integer(NA),
    deptID = as.integer(NA),
    group_name  = as.character(NA),
    status_inst = as.character(NA),
    status_dept = as.character(NA),
    date_info_mail = as.Date(NA),
    themes = NULL,
    initialize  = function(row) {
      conv <- private$initRowConv()
      self$year   <- row[[1, conv['year']]]
      self$ID     <- row[[1, conv['ID']]]
      self$instID <- row$instID
      self$deptID <- row$deptID
      self$group_name    <- row[[1, conv['groupName']]]
      self$status_inst  <- row$status_inst
      self$status_dept  <- row$status_dept
      self$date_info_mail <- row[[1, conv['date_info_mail']]]
      self$themes <- list()
      invisible(self)
    }, # end of init
    addTheme = function(theme) {
      is_acceptable <-
        classs(self) %>% stringr::str_detect("^ThemeOrg$") %>% any()
      if (!is_acceptable)
        stop(
          sprintf("Invalid Argument Error. ArgCls: %s", class(theme))
        )
      self$themes <- c(self$themes, theme)
    },
    inherit_classes = function() {
      cls <- private$.__class__
      cls_chain <- list()
      while (!is.null(cls)) {
        chls_chain[[length(cls_chain) + 1]] <- cls
        cls <- ifelse(is.symbol(cls$inherit), eval(cls$inherit), cls$inherit)
      }
      return(cls_chain)
    },
    isMemberOf = function(cls) {
      identical(private$.__class__, cls)
    },
    isKindOf   = function(cls) {
      self$inherit_classes() %>% purrr::map_lgl(~identical(., cls)) %>% any()
    }
  ), # end of public
  private = list(
    .__class__ = NULL,
    ## should over-ride
    init = function() {
      warning(msg_not_overridden(self, "init"))
    },
    ## should over-ride
    initRowConv = function() {
      warning(msg_not_overridden(self, "initRowConv"))
    }
  )
)
