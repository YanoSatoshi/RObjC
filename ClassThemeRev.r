#' ThemeRev Definition
ClassThemeRev <- R6::R6Class(
  ### class name ###
  "ThemeRev",
  inherit   = ClassThemeRoot,
  cloneable = FALSE,
  public = list(
    category = as.character(NA),
    phases = NULL,
    initialize = function(row) {
      self$category  <- row$ctg_theme_full
      self$condition <- row$theme_condition
      self$phases    <- list()
      invisible(self)
    },
    addPhase = function(phase) {
      is_acceptable <-
        class(phase) %>% stringr::str_detect("^RevPhase$") %>% any()
      if (!is_acceptable)
        stop(
          sprintf("Invalid Argument Error. ArgCls: %s", class(phase))
        )
      self$phases <- c(self$phases, phase)
    },
    print = function() {

    }
  ),
  private = list(
    initRowConv = function() {
      set_names(
        c('year', 'themeID', 'theme_inform_mail', 'theme_status'),
        'year', 'ID', 'date_info_mail', 'status'
      )
    }
  )
)