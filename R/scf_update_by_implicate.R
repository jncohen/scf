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
#' @section Use This When:
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
#' \dontrun{
#' scf2022 <- scf_load(2022)
#'
#' # Example 1: Flag top 10% of net worth within each implicate
#' scf2022 <- scf_update_by_implicate(scf2022, function(df) {
#'   cutoff <- quantile(df$networth, 0.9, na.rm = TRUE)
#'   df$top10 <- df$networth > cutoff
#'   df
#' })
#'
#' # Example 2: Percentile rank of income
#' scf2022 <- scf_update_by_implicate(scf2022, function(df) {
#'   df$income_pctile <- rank(df$income, na.last = "keep") / sum(!is.na(df$income))
#'   df
#' })
#'
#' # Example 3: Z-score by education group
#' scf2022 <- scf_update_by_implicate(scf2022, function(df) {
#'   mu <- ave(df$income, df$edcl, FUN = mean)
#'   sigma <- ave(df$income, df$edcl, FUN = sd)
#'   df$z_income <- (df$income - mu) / sigma
#'   df
#' })
#' }
#'
#' @seealso [scf_update()], [scf_extract_implicates()]
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
