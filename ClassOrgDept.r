#' OrgDept Definition
ClassOrgDept <- R6::R6Class(
  ### class name ###
  "OrgDept",
  inherit   = ClassOrgRoot,
  cloneable = FALSE,
  public = list(
    instID = as.integer(NA),
    themes_rev = NULL, # list[ThemeRev]
    themes_itv = NULL, # list[ThemeItv]
    initialize = function(row) {
      super$initialize(row)
      #
      invisible(self)
    },
    addThemeRev = function(theme) {
      req_class <- "ThemeRev"
      if (!isa_meber_of(theme, req_class))
        stop(msg_inv_arg_error(req_class, theme))
    },
    addThemeItv = function(theme) {
      req_class <- "ThemeItv"
      if (!isa_member_of(theme, req_class))
        stop(msg_inv_arg_error(req_class, theme))
    }
  ),
  private = list(
    initRowConv = function(){
      set_names(
        c('year', 'deptID', 'dept_full', 'dept_short'),
        'year', 'ID', 'name_full', 'name_short'
      )
    }
  )
)