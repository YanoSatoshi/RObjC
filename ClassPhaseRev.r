#' RevPhase Definition
ClassRevPhase <- R6::R6Class(
  ### class name ###
  "RevPhase",
  cloneable = FALSE,
  public = list(
    phaseID = as.integer(NA),
    themeID = as.integer(NA),
    date_document_rgst = as.Date(NA),
    date_review_upl = as.Date(NA),
    ctg_phase_full  = as.character(NA),
    review_section  = as.character(NA),
    phase_title     = as.character(NA),
    reviewer_name   = as.character(NA),
    initialize = function(row) {
      self$phaseID <- row$phaseID
      self$themeID <- row$themeID
      self$date_document_rgst <- row$date_document_rgst
      self$date_review_upl <- row$date_review_upl
      self$ctg_phase_full  <- row$ctg_phase_full
      self$review_section  <- row$review_section
      self$phase_title     <- row$phase_title
      self$reviewer_name   <- row$reviewer_name
      invisible(self)
    }
  )
)