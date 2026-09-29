load_dummies <- function() {
  path <- system.file("testdata/Dummies.r", package = "robjc")
  if (path == "") path <- file.path("inst", "testdata", "Dummies.r")
  if (!file.exists(path)) stop("Dummies.r not found at: ", path)
  source(path, local = .GlobalEnv)
}


unload_dummies <- function() {
  vars <- c("DummyClass", "NoVersionClass", "BadVersionClass")
  existing <- vars[sapply(vars, exists, envir = .GlobalEnv, inherits = FALSE)]
  if (length(existing) > 0) rm(list = existing, envir = .GlobalEnv)
}

