#' Construct an SCF Multiply-Imputed Survey Object
#'
#' @description
#' Wraps a list of replicate-weighted survey designs into an `scf_mi_survey`
#' object. This is called internally by [scf_load()], but is also available
#' directly for users who construct their own implicate-level designs outside
#' the standard download-and-load workflow — for example, when integrating
#' external or custom-prepared SCF data files.
#'
#' Each element of `design` must be a [survey::svrepdesign()] object representing
#' one SCF implicate with replicate weights.
#'
#' @param design A list of five [survey::svrepdesign()] objects (one per implicate).
#' @param year Numeric SCF survey year (e.g., 2022).
#' @param n_households Numeric total U.S. households represented in `year`.
#'
#' @return An object of class "scf_mi_survey" with:
#' \describe{
#'   \item{mi_design}{List of replicate-weighted designs (one per implicate).}
#'   \item{year}{SCF survey year.}
#'   \item{n_households}{Estimated number of U.S. households.}
#' }
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("design_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Construct scf_mi_survey object
#' obj <- scf_design(
#'   design = scf2022$mi_design,
#'   year = 2022,
#'   n_households = attr(scf2022, "n_households")
#' )
#' class(obj)
#' length(obj$mi_design)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @seealso [scf_load()], [scf_update()]
#' @export
scf_design <- function(design, year, n_households) {
  if (!is.list(design) || !all(sapply(design, inherits, "svyrep.design")))
    stop("`design` must be a list of `svyrep.design` objects (one per implicate).")
  structure(list(mi_design = design, year = year, n_households = n_households),
            class = "scf_mi_survey")
}

#' @export
print.scf_mi_survey <- function(x, ...) {
  cat("SCF Multiply-Imputed Survey Object\n")
  cat("----------------------------------\n")
  cat("Year:          ", x$year, "\n", sep = "")
  cat("Households (N):", format(x$n_households, big.mark = ","), "\n", sep = "")
  cat("Implicates:    ", length(x$mi_design), "\n", sep = "")
  cat("Replicate weights per implicate:",
      ncol(x$mi_design[[1]]$repweights), "\n")
  invisible(x)
}
