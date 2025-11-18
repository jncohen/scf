#' Estimate the Population Median of a Continuous SCF Variable
#'
#' @description
#' Estimates the median (50th percentile) of a continuous SCF variable. Use this
#' operation to characterize a typical or average value.  In contrast to
#' [scf_mean()], this function is both uninfluenced by, and insensitive to,
#' outliers.
#'
#' @section Implementation:
#' This function wraps [scf_percentile()] with `q = 0.5`. The user supplies a
#' `scf_mi_survey` object and a one-sided formula for the variable of interest,
#' with an optional grouping formula. Output includes pooled medians,
#' standard errors, min/max across implicates, and implicate-level values.
#' Point estimates are the mean of the five implicate medians. Standard errors
#' are computed using the Survey of Consumer Finances convention described
#' below, not Rubin’s Rules.
#' 
#' @section Statistical Notes:
#' Median estimates follow the Federal Reserve Board’s SCF variance convention.
#' For each implicate, the median is computed with replicate weights via
#' [survey::svyquantile()]. The pooled estimate is the average of the five
#' implicate medians. The pooled variance is
#'   V_total = V1 + ((m + 1) / m) * B,
#' where V1 is the replicate-weight sampling variance from the first implicate
#' and B is the between-implicate variance of the five implicate medians, with
#' m = 5 implicates. The reported standard error is sqrt(V_total). This matches
#' the Federal Reserve Board's published SAS macro for SCF descriptive
#' statistics and is not Rubin’s Rules.

#'
#' @param scf A `scf_mi_survey` object created by [scf_load()]. Must contain five implicates.
#' @param var A one-sided formula specifying the continuous variable of interest (e.g., `~networth`).
#' @param by Optional one-sided formula for a categorical grouping variable.
#' @param verbose Logical; if TRUE, show implicate-level results.
#'
#' @return A list of class `"scf_median"` with:
#' \describe{
#'   \item{results}{A data frame with pooled medians, standard errors, and range across implicates.}
#'   \item{imps}{A list of implicate-level results.}
#'   \item{aux}{Variable and grouping metadata.}
#' }
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td  <- tempdir()
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Estimate medians
#' scf_median(scf2022, ~networth)
#' scf_median(scf2022, ~networth, by = ~edcl)
#' 
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(file.path(td, "scf2022.rds"), force = TRUE)
#'
#' @seealso [scf_percentile()], [scf_mean()]
#'
#' @export
scf_median <- function(scf, var, by = NULL, verbose = FALSE) {
  out <- scf_percentile(scf, var, q = 0.5, by = by, verbose = verbose)
  out$aux$quantile <- NULL
  class(out) <- c("scf_median", "scf_percentile")
  out
}


#' @export
print.scf_median <- function(x, ...) {
  cat("Multiply-Imputed Median Estimate\n\n")
  print(x$results, row.names = FALSE, ...)
  if (isTRUE(x$verbose)) {
    cat("\nImplicate-Level Estimates:\n\n")
    imp_df <- do.call(rbind, x$imps)
    print(imp_df, row.names = FALSE)
  }
  invisible(x)
}

#' @export
summary.scf_median <- function(object, ...) {
  cat("Summary of Multiply-Imputed Median Estimate\n\n")
  cat("Pooled Estimates:\n")
  print(format(object$results, digits = 4, nsmall = 2), row.names = FALSE, ...)
  cat("\nImplicate-Level Estimates:\n")
  imp_df <- do.call(rbind, object$imps)
  print(format(imp_df, digits = 4, nsmall = 2), row.names = FALSE)
  invisible(object)
}
