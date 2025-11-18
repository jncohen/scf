#' Modify Each Implicate Individually in SCF Data
#'
#' Applies a user-defined transformation to each implicate's data frame separately.
#' This is useful when you need to compute values that depend on the distribution
#' within each implicate — such as ranks, percentiles, or groupwise comparisons —
#' which cannot be computed reliably using [scf_update()].
#'
#' @description
#' Each household in SCF data is represented by five *implicates*, which reflect
#' uncertainty from the imputation process. Most transformations — such as computing
#' log income or assigning categorical bins — can be applied uniformly across implicates
#' using [scf_update()]. However, some operations depend on the *internal distribution*
#' of variables within each implicate. For those, you need to modify each one separately.
#'
#' This function extracts each implicate from the replicate-weighted survey design,
#' applies your transformation, and rebuilds the survey design objects accordingly.
#'
#' @section Use this When:
#' - You need implicate-specific quantiles (e.g., flag households in the top 10% of wealth)
#' - You want to assign percentile ranks (e.g., income percentile by implicate)
#' - You are computing statistics within groups (e.g., groupwise z-scores)
#' - You need to derive a variable based on implicate-specific thresholds or bins
#'
#' @param object A `scf_mi_survey` object from [scf_load()].
#' @param f A function that takes a data frame as input and returns a modified data frame.
#'   This function will be applied independently to each implicate.
#'
#' @return A modified `scf_mi_survey` object with updated implicate-level designs.
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td  <- tempdir()
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: compute implicate-specific z-scores of income
#' scf2022 <- scf_update_by_implicate(scf2022, function(df) {
#'   mu <- mean(df$income, na.rm = TRUE)
#'   sigma <- sd(df$income, na.rm = TRUE)
#'   df$z_income <- (df$income - mu) / sigma
#'   df
#' })
#'
#' # Verify new variable exists
#' head(scf2022$mi_design[[1]]$variables$z_income)
#' 
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(file.path(td, "scf2022.rds"), force = TRUE)
#'
#' @seealso [scf_update()]
#' @export
scf_update_by_implicate <- function(object, f) {
  if (!inherits(object, "scf_mi_survey")) {
    stop("Input must be a 'scf_mi_survey' object.")
  }
  if (!is.function(f)) {
    stop("Argument 'f' must be a function that accepts and returns a data frame.")
  }
  
  # Extract implicates from replicate-weighted survey designs
  implicates <- lapply(object$mi_design, function(design) design$variables)
  
  updated_implicates <- vector("list", length(implicates))
  updated_designs <- vector("list", length(implicates))
  
  for (i in seq_along(implicates)) {
    df <- implicates[[i]]
    new_df <- f(df)
    
    if (!is.data.frame(new_df)) {
      stop(sprintf("Function `f` must return a data.frame. Implicate %d returned: %s",
                   i, class(new_df)))
    }
    if (nrow(new_df) != nrow(df)) {
      stop(sprintf("Row count mismatch in implicate %d: original = %d, new = %d",
                   i, nrow(df), nrow(new_df)))
    }
    
    rep_cols <- grep("^wt1b", names(new_df), value = TRUE)
    if (length(rep_cols) == 0) {
      stop("Could not find replicate weight columns in implicate.")
    }
    
    updated_implicates[[i]] <- new_df
    updated_designs[[i]] <- survey::svrepdesign(
      weights = ~wgt,
      repweights = new_df[, rep_cols],
      data = new_df,
      type = "BRR",
      fay.rho = 0.5,
      mse = TRUE,
      combined.weights = TRUE
    )
  }
  
  object$mi_design <- updated_designs
  return(object)
}
