#' Construct SCF Core Data Object
#'
#' This is a helper function for the [scf_download()] and [scf_load()] functions. 
#' Wrap a list of replicate-weighted survey designs into an "scf_mi_survey".
#' Typically called by [scf_load()]. The function creates a complex object that
#' includes the Survey's five implicates, along with the year and an 
#' estimate of the total U.S. households in that year.
#'
#' @description
#' Stores SCF microdata as five implicate-specific designs created by
#' [survey::svrepdesign()].
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
#' # Ignore this code block.  It loads mock data for CRAN.
#' # In your analysis, download and load your data using the
#' # functions `scf_download()` and `scf_load()`
#' td  <- tempdir()
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # EXAMPLE IMPLEMENTATION: Construct scf_mi_survey object
#' obj <- scf_design(
#'   design = scf2022$mi_design,
#'   year = 2022,
#'   n_households = attr(scf2022, "n_households")
#' )
#' class(obj)
#' length(obj$mi_design)
#' 
#' # Ignore the code below.  It is for CRAN:
#' unlink("scf2022.rds", force = TRUE)
#'
#' @seealso [scf_load()], [scf_update()]
#' @export
scf_design <- function(design, year, n_households) {
  if (!is.list(design) || !all(sapply(design, inherits, "svyrep.design")))
    stop("`design` must be a list of `svrep.design` objects (one per implicate).")
  structure(list(mi_design = design, year = year, n_households = n_households),
            class = "scf_mi_survey")
}
