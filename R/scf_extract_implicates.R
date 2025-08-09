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
#' @seealso [scf_update_by_implicate()], [scf_load()]
#' @export
scf_extract_implicates <- function(object) {
  if (!inherits(object, "scf_mi_survey")) stop("Input must be of class 'scf_mi_survey'.")
  imps <- lapply(object$mi_design, function(d) d$variables)
  names(imps) <- paste0("set", seq_along(imps))
  imps
}
