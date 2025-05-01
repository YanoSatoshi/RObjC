if (!exists('EnvManagers')) {
  ### NEW ENV
  EnvManagers <- new.env(parent = .GlobalEnv)
  
  assign(envir = EnvManagers,
    "ClassManager",
    R6::R6Class(
      ### class nmae ###
      "Manager",
      cloneable = FALSE,
      public = list(
        managing = as.character(NA),
        objects = NULL,
        initialize = function(managing_class) {

        },
        summary = function() { # __str__ in python

        }
      )
    )
  )
}