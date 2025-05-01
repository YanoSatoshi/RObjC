#' OrgInst Definition
RSOrgInst <- R6::R6Class(
  ### class name ###
  "RSOrgInst",
  inherit   = ClassOrgRoot,
  cloneable = FALSE,
  public = list(
    xml_objs = NULL, # list[XmlInst]
    depts    = NULL, # list[OrgDept]
    initialize = function(row) {
      super$initialize(row)
      self$depts <- list()
      invisible(self)
    },
    addDept = function(dept) {
      req_class <- "OrgDept"
      if (!isa_member_of(dept, req_class))
        stop(msg_inv_arg_error(dept, req_class))
      self$depts <- c(self$depts, dept)
    }
  ),
  private = list(
    initRowConv = function() {
      set_names(
        c('year', 'instID', 'inst_full', 'inst_short'),
        'year', 'ID', 'name_full', 'name_short'
      )
    }
  )
)