#' RSXmlInst Definition
RSXmlInst <- R6::R6Class(
  ### class name ###
  "RSXmlInst",
  cloneable = FALSE,
  public = list(
    group = as.integer(NA),
    code_mext = as.character(NA),
    code_nii  = as.character(NA),
    code_jsps = as.character(NA),
    name_ja   = as.character(NA),
    name_en   = as.character(NA),
    name_yomi = as.character(NA),
    since = as.Date(NA),
    until = as.Date(NA),
    initialize = function(df) {
      self$group     <- df$group
      self$code_mext <- df$code_mext
      self$code_nii  <- df$code_nii
      self$code_jsps <- df$code_jsps
      self$name_ja   <- df$name_ja
      self$name_en   <- df$name_en
      self$name_yomi <- df$name_yomi
      self$since <- df$since
      self$until <- df$until
      invisible(self)
    },
    #' @description Category which the institute belongs to.
    #' @return Category item in Ja (`factor`)
    fct_group  = function() {
      if(is.na(self$code_mext)) return(NA);
      KAKEN_GROUP_LABEL %>%
        dplyr::filter(code == self$group) %>%
        .$label_ja
    },
    #' @description Location (prefecture) of the institute.
    #' @return Prefecture name in Ja (`character`).\cr
    #' Because Tokyo Pref is expressed in two codes, not factorized.\cr
    str_pref   = function() {
      if(is.na(self$code_mext)) return(NA);
      pref_code <- stringr::str_extract(self$code_mext, "(?<=^\\d)\\d{2}")
      KAKEN_PREF_LABEL %>%
        dplyr::filter(code == pref_code) %>%
        .$pref
    },
    #' @description Did the institute exist at .year?
    #' @param .year (`int`) Fiscal year
    #' @return Logical/Boolean value
    exists_at_yr = function(.year) {
      if (is.na(self$code_mext)) return(NA);
      year <- paste0(.year, "-04-02") %>% lubridate::ymd()
      until  <- ifelse(is.na(self$until), today(), self$until)
      period <- lubridate::interval(self$since, until)
      return(year %within% period)
    }
  )
)
