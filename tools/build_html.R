# Run from the package root with R.app's Rscript to refresh html/.
# R CMD INSTALL creates static help pages; this script makes same-package links
# usable when the pages are opened directly from the source checkout.
local({
  root <- normalizePath(getwd())
  if (!file.exists(file.path(root, "DESCRIPTION")))
    stop("Run this script from the package root.")

  package <- read.dcf(file.path(root, "DESCRIPTION"), fields = "Package")[[1L]]
  library_dir <- tempfile("robjc-html-library-")
  dir.create(library_dir)
  on.exit(unlink(library_dir, recursive = TRUE), add = TRUE)

  r <- file.path(R.home("bin"), "R")
  status <- system2(
    r,
    c("CMD", "INSTALL", "--html", "--use-vanilla",
      paste0("--library=", shQuote(library_dir)), shQuote(root))
  )
  if (!identical(status, 0L)) stop("R CMD INSTALL --html failed.")

  installed <- file.path(library_dir, package)
  source_html <- file.path(installed, "html")
  output_html <- file.path(root, "html")
  if (dir.exists(output_html)) unlink(output_html, recursive = TRUE)
  dir.create(output_html)
  files <- list.files(source_html, full.names = TRUE)
  if (!all(file.copy(files, output_html)))
    stop("Could not copy all HTML files.")

  pages <- list.files(output_html, pattern = "[.]html$", full.names = TRUE)
  for (page in pages) {
    lines <- readLines(page, warn = FALSE, encoding = "UTF-8")
    lines <- gsub(paste0("../../", package, "/html/"), "", lines, fixed = TRUE)
    lines <- gsub(paste0("../../", package, "/help/"), "", lines, fixed = TRUE)
    lines <- gsub("../help/", "", lines, fixed = TRUE)
    lines <- gsub("../../base/help/",
                   "https://stat.ethz.ch/R-manual/R-devel/library/base/help/",
                   lines, fixed = TRUE)
    lines <- gsub("../../R6/help/",
                   "https://search.r-project.org/CRAN/refmans/R6/html/",
                   lines, fixed = TRUE)
    lines <- gsub("../../../doc/html/packages.html", "00Index.html",
                   lines, fixed = TRUE)
    lines <- gsub("../../../doc/html/index.html", "00Index.html",
                   lines, fixed = TRUE)
    lines <- gsub('<img class="toplogo" src="../../../doc/html/Rlogo.svg" alt="[R logo]" />',
                   "", lines, fixed = TRUE)
    writeLines(lines, page, useBytes = TRUE)
  }

  # R's installed help server resolves aliases, but local HTML files need them.
  aliases <- readRDS(file.path(installed, "help", "aliases.rds"))
  contents <- vapply(
    pages,
    function(page) paste(readLines(page, warn = FALSE), collapse = "\n"),
    character(1)
  )
  hrefs <- unlist(regmatches(
    contents, gregexpr("href=['\"][^'\"]+['\"]", contents, perl = TRUE)
  ), use.names = FALSE)
  hrefs <- sub("^href=['\"]", "", hrefs)
  hrefs <- sub("['\"]$", "", hrefs)
  hrefs <- sub("#.*$", "", hrefs)
  local_targets <- unique(hrefs[grepl("^[[:alnum:]_.+-]+[.]html$", hrefs)])
  for (target in local_targets) {
    if (file.exists(file.path(output_html, target))) next
    alias <- sub("[.]html$", "", target)
    if (!alias %in% names(aliases))
      stop("Unresolved local HTML link: ", target)
    source <- file.path(output_html, paste0(aliases[[alias]], ".html"))
    if (!file.copy(source, file.path(output_html, target)))
      stop("Could not create HTML alias: ", target)
  }

  message("Static help written to ", output_html)
})
