#' Modify Each Implicate Individually in SCF Data
#'
#' This function lets you make changes to each implicate in an SCF object by
#' applying a function that modifies a full data frame. It’s useful when the
#' transformation you want to make depends on values that are different in
#' each implicate — for example, percentiles, ranks, or group summaries.
#'
#' @description
#' In the SCF, each household is represented by five slightly different
#' "implicates" — five versions of the data that reflect uncertainty in the
#' imputation process. Most of the time, you can modify SCF data using
#' [scf_update()], which lets you define new variables the usual way:
#' for example, `rich = networth > 1e6` or `log_income = log(income)`.
#'
#' But sometimes, what you want to compute depends on values that vary
#' from one implicate to another — like computing the 90th percentile
#' within each implicate, or ranking households by income separately in
#' each version. In those cases, `scf_update()` won’t work correctly,
#' because it assumes that your transformation is the same across implicates.
#'
#' `scf_update_by_implicate()` solves this by letting you write a function
#' that operates directly on each implicate's data frame. You give it a
#' function that takes a data frame as input and returns a modified version.
#' That function is applied to all five implicates separately.
#'
#' @section When to Use:
#' Use `scf_update_by_implicate()` when:
#' - Your transformation depends on the distribution of a variable within each implicate
#' - You want to compute a percentile, rank, z-score, or group summary
#' - You need full access to the data frame to write your logic
#'
#' Use [scf_update()] when:
#' - You're computing something simple like a flag, a log, or a cutpoint
#' - The rule doesn't change across implicates
#'
#' If you're not sure, start with `scf_update()`. If it fails, or if your
#' results look wrong, try `scf_update_by_implicate()` instead.
#'
#' @param object A `scf_mi_survey` object (from [scf_load()]) containing SCF implicates.
#' @param f A function that takes a data frame as input and returns a modified data frame.
#'   This function will be applied to each implicate separately.
#'
#' @return A modified `scf_mi_survey` object, with updated implicates and survey designs.
#'
#' @examples
#' # Load mock SCF data
#' scf2022 <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))
#'
#' # Example: Flag households in the top 10% of net worth (separately in each implicate)
#' scf2022 <- scf_update_by_implicate(scf2022, function(df) {
#'   p90 <- quantile(df$networth, 0.9, na.rm = TRUE)
#'   df$top10 <- df$networth > p90
#'   df
#' })
#'
#' # Check top10 flag in implicate 1
#' table(scf2022$implicates[[1]]$top10, useNA = "ifany")
#'
#' @seealso [scf_update()], [scf_load()], [survey::svrepdesign()]
#' @export
scf_update_by_implicate <- function(object, f) {
  if (!inherits(object, "scf_mi_survey")) {
    stop("Input must be of class 'scf_mi_survey'.")
  }
  if (!is.function(f)) {
    stop("`f` must be a function that accepts a data frame and returns a modified data frame.")
  }

  updated_implicates <- vector("list", length(object$implicates))
  updated_designs <- vector("list", length(object$implicates))

  for (i in seq_along(object$implicates)) {
    df <- object$implicates[[i]]
    new_df <- f(df)

    if (!is.data.frame(new_df)) {
      stop(sprintf("Function `f` must return a data.frame. Implicate %d returned: %s",
                   i, class(new_df)))
    }
    if (nrow(new_df) != nrow(df)) {
      stop(sprintf("Row count mismatch in implicate %d: original = %d, new = %d",
                   i, nrow(df), nrow(new_df)))
    }

    updated_implicates[[i]] <- new_df

    rep_cols <- grep("^wt1b", names(new_df), value = TRUE)
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

  object$implicates <- updated_implicates
  object$mi_design <- updated_designs

  return(object)
}
