#' Extract Raw Implicates into Named Data Frames
#'
#' @description
#' Extracts the five implicate data frames embedded within a light `scf_mi_survey` object
#' and places them into the global environment with numbered suffixes (e.g., `scf2022_1`,
#' `scf2022_2`, ..., `scf2022_5`).
#'
#' @param object An `scf_mi_survey` object created by [scf_load()]. Must be light (no `$implicates`).
#' @param label Optional base name for the output objects. Defaults to the name of `object`.
#'
#' @return Invisibly returns a list of the five implicate data frames (invisibly).
#'
#' @details
#' This function is used to extract user-facing data frames from the internal
#' `svrepdesign` objects in a light SCF object. The extracted implicates are placed
#' in the global environment for inspection or transformation.
#'
#' @examples
#' \donttest{
#' # Note mock data used here is for testing purposes only.
#' 
#' scf2022 <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))
#' imps <- scf_extract_implicates(scf2022, label = "mydata")
#' scf_extract_implicates(scf2022)
#' head(scf2022_1$income)
#'
#' # Custom base label
#' scf_extract_implicates(scf2022, "imps")
#' head(imps_1$income)
#' }
#'
#' @seealso [scf_update_by_implicate()], [scf_load()]
#' @export
scf_extract_implicates <- function(object, label = NULL) {
  if (!inherits(object, "scf_mi_survey")) stop("Input must be of class 'scf_mi_survey'.")
  if (is.null(label)) label <- deparse(substitute(object))
  
  imps <- lapply(object$mi_design, function(d) d$variables)
  names(imps) <- paste0(label, "_", seq_along(imps))
  return(imps)
}

