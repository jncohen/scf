# zzz.R

.onLoad <- function(libname, pkgname) {
  ggplot2::theme_set(scf_theme())

  # Only copy test data interactively or in local dev, not during CRAN build/check
  # if (interactive() || identical(Sys.getenv("NOT_CRAN"), "true")) {
  #  source_path <- system.file("testdata/scf2022.rds", package = pkgname)
  #  dest_path <- file.path(getwd(), "scf2022.rds")
  #  if (file.exists(source_path) && !file.exists(dest_path)) {
  #    file.copy(source_path, dest_path, overwrite = FALSE)
  #  }
  # }
}

# Register global variables to silence CMD check notes
utils::globalVariables(c(
  "group", "category", "estimate", "yval", "xval", "wgt", "x",
  "percent", "proportion", "imp", "weights"
))
