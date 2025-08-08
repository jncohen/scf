#' Construct SCF Core Data Object
#'
#' Wraps a list of replicate-weighted survey designs into an `"scf_mi_survey"` object
#' used throughout the `scf` package. Typically called by [scf_load()], but also
#' available for constructing custom survey objects manually.  See Cohen (2025)
#' for further details.
#'
#' @description
#' This function creates a complex data object that stores SCF microdata as a
#' list of objects for each of the set's five implicates.  The function is used
#' in [scf_load()] to load the SCF data and create survey design objects using
#' `survey::svrep.design()`.
#'
#' @section Implementation:
#' Accepts a list of five `survey::svrep.design` objects and (optionally) their underlying
#' data frames. This structure allows for standardized processing of replicate-weighted,
#' multiply-imputed SCF data in a form compatible with internal `scf` functions.
#'
#' @section Details:
#' The object returned is a list of class `"scf_mi_survey"` with implicate-specific
#' designs, optional raw data, survey metadata, and population totals. 
#'
#' @param design A list of five `svrep.design` objects (one for each implicate), created using [survey::svrepdesign()].
#' @param implicates Optional list of five data frames, each corresponding to one implicate’s raw data.
#' @param year Numeric. The SCF survey year (e.g., `2022`).
#' @param n_households Numeric. Total number of households represented in the U.S. in `year` per FRED data.
#'
#' @return An object of class `"scf_mi_survey"` containing:
#' \describe{
#'   \item{mi_design}{List of replicate-weighted designs (one per implicate).}
#'   \item{implicates}{(Optional) Raw implicate-level data frames.}
#'   \item{year}{SCF survey year.}
#'   \item{n_households}{Estimated number of U.S. households.}
#' }
#'
#' @examples
#' \dontrun{
#' obj <- scf_design(
#'   design = list(imp1, imp2, imp3, imp4, imp5),
#'   implicates = list(df1, df2, df3, df4, df5),
#'   year = 2022,
#'   n_households = 131202000
#' )
#' }
#'
#' @seealso [scf_load()], [scf_update()], [`survey::svrepdesign()`]
#'
#' @references
#' Barnard, J., & Rubin, D. B. (1999). "Small-sample degrees of freedom with multiple imputation." *Biometrika*, 86(4), 948–955.
#'
#' Bricker, Jesse, Alice M. Henriques, and Kevin B. Moore. 2017. Updates to the Sampling of Wealthy Families in the Survey of Consumer Finances. 2017–114. Finance and Economics Discussion Series. https://www.federalreserve.gov/econres/feds/updates-to-the-sampling-of-wealthy-families-in-the-survey-of-consumer-finances.htm.
#'
#' Cohen, Joseph N. (2025) "The `scf` Package: Analyzing the Survey of Consumer Finances in R". Manuscript in Review.
#'
#' @export
scf_design <- function(design, implicates = NULL, year, n_households) {
  if (!is.list(design) || !all(sapply(design, inherits, what = "svyrep.design"))) {
    stop("`design` must be a list of `svrep.design` survey objects (one per implicate).")
  }

  structure(
    list(
      mi_design = design,           # Now a simple list, not a pooled object
      implicates = implicates,
      year = year,
      n_households = n_households
    ),
    class = "scf_mi_survey"
  )
}
