#' Construct SCF Core Data Object
#'
#' Wrap a list of replicate-weighted survey designs into an "scf_mi_survey".
#' Typically called by [scf_load()].
#'
#' @description
#' Stores SCF microdata as five implicate-specific designs created by
#' [survey::svrepdesign()]. Raw implicate data frames are not retained.
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
#' \dontrun{
#' obj <- scf_design(
#'   design = list(imp1, imp2, imp3, imp4, imp5),
#'   year = 2022,
#'   n_households = 131202000
#' )
#' }
#'
#' @seealso [scf_load()], [scf_update()]
#' @export
scf_design <- function(design, year, n_households) {
  if (!is.list(design) || !all(sapply(design, inherits, "svyrep.design")))
    stop("`design` must be a list of `svrep.design` objects (one per implicate).")
  structure(list(mi_design = design, year = year, n_households = n_households),
            class = "scf_mi_survey")
}
