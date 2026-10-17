#' Construct an SCF Multiply-Imputed Survey Object
#'
#' Wraps a list of implicate designs into an `scf_mi_survey` object.
#'
#' @details
#' [scf_load()] calls this function. Call it directly only when building
#' implicate designs outside the download-and-load workflow. Each element of
#' `design` must be a [survey::svrepdesign()] object.
#'
#' @param design A list of five [survey::svrepdesign()] objects (one per implicate).
#' @param year Numeric SCF survey year (e.g., 2022).
#' @param n_households Numeric total U.S. households represented in `year`.
#' @param dollar_year Year whose dollars the Fed's summary variables, such as
#'   `income` and `networth`, are in. If `NULL`, it is found from the data by
#'   comparing `wageinc` with `x5702`.
#'
#' @return An object of class "scf_mi_survey" with:
#' \describe{
#'   \item{mi_design}{List of replicate-weighted designs (one per implicate).}
#'   \item{year}{SCF survey year.}
#'   \item{n_households}{Estimated number of U.S. households.}
#'   \item{dollar_year}{Year whose dollars the summary variables are in, or
#'     `NA` if unknown.}
#' }
#'
#' @examples
#' \dontshow{
#' td <- tempfile()
#' dir.create(td)
#' file.copy(system.file("extdata", "scf2022_mock_raw.rds", package = "scf"),
#'           file.path(td, "scf2022.rds"))
#' scf2022 <- scf_load(2022, data_directory = td)
#' }
#' # Construct scf_mi_survey object
#' obj <- scf_design(
#'   design = scf2022$mi_design,
#'   year = 2022,
#'   n_households = scf2022$n_households
#' )
#' class(obj)
#' length(obj$mi_design)
#' \dontshow{unlink(td, recursive = TRUE, force = TRUE)}
#'
#' @seealso [scf_load()], [scf_update()]
#' @export
scf_design <- function(design, year, n_households, dollar_year = NULL) {
  if (!is.list(design) || !all(sapply(design, inherits, "svyrep.design")))
    stop("`design` must be a list of `svyrep.design` objects (one per implicate).")
  if (is.null(dollar_year)) {
    dollar_year <- .scf_detect_dollar_year(design[[1]]$variables, year)
  }
  structure(list(mi_design = design, year = year, n_households = n_households,
                 dollar_year = as.integer(dollar_year)),
            class = "scf_mi_survey")
}

#' @export
print.scf_mi_survey <- function(x, ...) {
  cat("SCF Multiply-Imputed Survey Object\n")
  cat("----------------------------------\n")
  cat("Year:           ", x$year, "\n", sep = "")
  cat("Households (N): ", format(x$n_households, big.mark = ","), "\n", sep = "")
  cat("Implicates:     ", length(x$mi_design), "\n", sep = "")
  cat("Replicate weights per implicate:",
      ncol(x$mi_design[[1]]$repweights), "\n")
  if (!is.null(x$dollar_year) && !is.na(x$dollar_year)) {
    cat("Dollar year:    ", x$dollar_year, "\n", sep = "")
  }
  invisible(x)
}
