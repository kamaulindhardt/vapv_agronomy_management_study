source("renv/activate.R")

# Activate a pandoc install managed by the `pandoc` R package, if present.
# rmarkdown::render() (used by tarchetypes::tar_render() for the Study 2
# report) needs pandoc on PATH or via RSTUDIO_PANDOC; the `pandoc` package's
# own activation is session-scoped, so it has to be redone on every R
# startup here rather than once. If `pandoc` isn't installed yet, run:
#   install.packages("pandoc"); pandoc::pandoc_install()
# Wrapped in tryCatch(): this runs on EVERY R startup in this project,
# including build subprocesses spawned by install.packages()/renv, so it
# must never itself error or it silently breaks unrelated package builds.
try(silent = TRUE, {
  if (requireNamespace("pandoc", quietly = TRUE)) {
    loc <- pandoc::pandoc_locate()
    if (!is.null(loc) && nzchar(loc)) Sys.setenv(RSTUDIO_PANDOC = loc)
  }
})
