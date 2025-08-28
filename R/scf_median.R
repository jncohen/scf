#' Estimate the Population Median of a Continuous SCF Variable
#'
#' @description
#' Estimates the median (50th percentile) of a continuous SCF variable. Use this
#' operation to characterize a typical or average value.  In contrast to
#' [scf_mean()], this function is both uninfluenced by, and insensitive to,
#' outliers.
#'
#' @section Implementation:
#' This function wraps [scf_percentile()] with `q = 0.5`. The user provides a
#' `scf_mi_survey` object and a one-sided formula indicating the variable of
#' interest. An optional grouping variable can be specified with a second
#' formula.  Output includes pooled medians, standard errors, min/max across
#' implicates, and implicate-level values.
#'
#' @section Statistical Notes:
#' Median estimates are not pooled using Rubin’s Rules. Following SCF protocol,
#' the function calculates the median within each implicate and averages across implicates.
#' See the data set's official codebook. 
#'
#' @param scf A `scf_mi_survey` object created by [scf_load()]. Must contain five implicates.
#' @param var A one-sided formula specifying the continuous variable of interest (e.g., `~networth`).
#' @param by Optional one-sided formula for a categorical grouping variable.
#'
#' @return A list of class `"scf_median"` with:
#' \describe{
#'   \item{results}{A data frame with pooled medians, standard errors, and range across implicates.}
#'   \item{imps}{A list of implicate-level results.}
#'   \item{aux}{Variable and grouping metadata.}
#' }
#'
#' @examples
#' # Load bundled mock data (for demonstration only — not real SCF data)
#' scf2022 <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))

#' # For real analysis, use:
#' # scf_download(2022); scf2022 <- scf_load(2022)
#'
#' scf_median(scf2022, ~networth)
#' scf_median(scf2022, ~networth, by = ~edcl)
#' 
#'
#' @seealso [scf_percentile()], [scf_mean()]
#'
#' @export
scf_median <- function(scf, var, by = NULL) {

  out <- scf_percentile(scf, var, q = 0.5, by = by)
  out$aux$quantile <- NULL
  class(out) <- c("scf_median", "scf_percentile")
  return(out)
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
