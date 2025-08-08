#' Extract Raw Implicates into a Structured List
#'
#' @description
#' Extracts the five implicate data frames embedded within an `scf_mi_survey` object
#' and returns them as a structured list with named elements (`set1`, `set2`, ..., `set5`).
#'
#' @param object An `scf_mi_survey` object created by [scf_load()].
#'
#' @return A named list of five implicate data frames, accessible as `list$set1`, `list$set2`, etc.
#' 
#' @details
#' This function extracts user-facing data frames from the internal
#' `svrepdesign` objects stored in an `scf_mi_survey` object. It returns a structured,
#' nested list suitable for inspection, transformation, or custom analysis workflows.
#'
#' @examples
#' \donttest{
#' # Load mock SCF data (for demonstration only)
#' scf2022 <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))
#'
#' # Extract implicates as a structured list
#' imps <- scf_extract_implicates(scf2022)
#'
#' # Access income variable from first implicate
#' head(imps$set1$income)
#'
#' # Loop over implicates for a custom analysis
#' sapply(imps, function(df) mean(df$age, na.rm = TRUE))
#' }
#'
#' @seealso [scf_update_by_implicate()], [scf_load()]
#' @export
scf_extract_implicates <- function(object) {
  if (!inherits(object, "scf_mi_survey")) {
    stop("Input must be of class 'scf_mi_survey'.")
  }
  
  imps <- lapply(object$mi_design, function(d) d$variables)
  names(imps) <- paste0("set", seq_along(imps))
  
  return(imps)
}
