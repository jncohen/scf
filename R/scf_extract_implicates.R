<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 71f218d525c427d63c21d653a6883f6858cbff86
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
<<<<<<< HEAD
=======
=======
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
>>>>>>> 6ead2c7b9f57c6c5bf94ccc5b3845a9d6ba259c6
>>>>>>> 71f218d525c427d63c21d653a6883f6858cbff86
#' }
#'
#' @seealso [scf_update_by_implicate()], [scf_load()]
#' @export
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 71f218d525c427d63c21d653a6883f6858cbff86
scf_extract_implicates <- function(object) {
  if (!inherits(object, "scf_mi_survey")) stop("Input must be of class 'scf_mi_survey'.")
  
  imps <- lapply(object$mi_design, function(d) d$variables)
  names(imps) <- paste0("set", seq_along(imps))
  return(imps)
}
<<<<<<< HEAD
=======
=======
scf_extract_implicates <- function(object, label = NULL) {
  if (!inherits(object, "scf_mi_survey")) stop("Input must be of class 'scf_mi_survey'.")
  if (is.null(label)) label <- deparse(substitute(object))
  
  imps <- lapply(object$mi_design, function(d) d$variables)
  names(imps) <- paste0(label, "_", seq_along(imps))
  return(imps)
}

>>>>>>> 6ead2c7b9f57c6c5bf94ccc5b3845a9d6ba259c6
>>>>>>> 71f218d525c427d63c21d653a6883f6858cbff86
