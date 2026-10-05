#' Estimate the Population Median of a Continuous SCF Variable
#'
#' Estimates the weighted median of a continuous variable, overall or by group.
#'
#' @details
#' Use it for skewed variables such as wealth and income, where it describes
#' the typical household better than the mean. Half of households are below
#' the estimate and half above. Dollar amounts are in 2022 dollars.
#'
#' Calls [scf_percentile()] with `q = 0.5`. The estimate is the mean of the
#' five implicate medians; the standard error combines sampling and imputation
#' variance (see [scf_variance]).
#'
#' @param scf A `scf_mi_survey` object created by [scf_load()]. Must contain five implicates.
#' @param var A one-sided formula specifying the continuous variable of interest (e.g., `~networth`).
#' @param by Optional one-sided formula for a categorical grouping variable.
#' @param verbose Logical; if TRUE, show implicate-level results.
#' @param variance Variance method: `"fed"` (default) or `"rubin"`. See [scf_variance].
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
#' td <- tempfile("median_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Estimate medians
#' scf_median(scf2022, ~networth)
#' scf_median(scf2022, ~networth, by = ~edcl)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @seealso [scf_percentile()], [scf_mean()]
#'
#' @export
scf_median <- function(scf, var, by = NULL, verbose = FALSE,
                       variance = getOption("scf.variance", "fed")) {
  out <- scf_percentile(scf, var, q = 0.5, by = by, verbose = verbose,
                        variance = variance)
  out$aux$quantile <- NULL
  class(out) <- c("scf_median", "scf_percentile")
  out
}


#' @export
print.scf_median <- function(x, ...) {
  dollar_label <- if (isTRUE(attr(x, "nominal")))
    sprintf(" (nominal %d dollars)", x$aux$year) else ""
  cat(sprintf("Multiply-Imputed Median Estimate%s\n\n", dollar_label))
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
