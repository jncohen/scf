#' Estimate the Population Median of a Continuous SCF Variable
#'
#' Estimates the weighted median of a continuous variable, overall or by group.
#'
#' @details
#' Use it for skewed variables such as wealth and income, where it describes
#' the typical household better than the mean. Half of households are below
#' the estimate and half above. Summary dollar variables, such as
#' `networth`, are in the data's dollar year (see [scf_load()]); raw `x`
#' variables are in nominal dollars (see [scf_real()]).
#'
#' Calls [scf_percentile()] with `q = 0.5`. With `method = "implicate"`, the
#' estimate is the mean of the five implicate medians; the standard error
#' combines sampling and imputation variance (see [scf_variance]). With
#' `method = "stack"`, the five implicates are stacked and one weighted median
#' is taken, as in the Fed's published tables; no standard error is given.
#' The two can differ in small groups.
#'
#' @param scf A `scf_mi_survey` object from [scf_load()].
#' @param var A one-sided formula specifying the continuous variable of interest (e.g., `~networth`).
#' @param by Optional one-sided formula for a categorical grouping variable.
#' @param verbose Logical; if TRUE, show implicate-level results.
#' @param method `"implicate"` (default) or `"stack"`. See Details.
#' @param variance `"fed"` (default) or `"rubin"`: how the sampling variance
#'   is combined across implicates. See [scf_variance]. Most users can leave
#'   this alone.
#'
#' @return A list of class `c("scf_median", "scf_percentile")` with:
#' \describe{
#'   \item{results}{A data frame with pooled medians (`quantile` is 0.5),
#'   standard errors, and range across implicates. With `method = "stack"`,
#'   `se`, `min`, and `max` are `NA` and `imps` is `NULL`.
#'   `n` is the number of sample households (unweighted) in each row,
#'   averaged over implicates.
#'   Groups of 10 or fewer households give a warning.}
#'   \item{imps}{A list of implicate-level results.}
#'   \item{aux}{Variable and grouping metadata.}
#' }
#'
#' @examples
#' \dontshow{
#' td <- tempfile()
#' dir.create(td)
#' file.copy(system.file("extdata", "scf2022_mock_raw.rds", package = "scf"),
#'           file.path(td, "scf2022.rds"))
#' scf2022 <- scf_load(2022, data_directory = td)
#' }
#' # Estimate medians
#' scf_median(scf2022, ~networth)
#' scf_median(scf2022, ~networth, by = ~edcl)
#' scf_median(scf2022, ~networth, method = "stack")
#' \dontshow{unlink(td, recursive = TRUE, force = TRUE)}
#'
#' @seealso [scf_percentile()], [scf_mean()]
#'
#' @export
scf_median <- function(scf, var, by = NULL, verbose = FALSE,
                       method = c("implicate", "stack"),
                       variance = "fed") {
  method <- match.arg(method)
  out <- scf_percentile(scf, var, q = 0.5, by = by, verbose = verbose,
                        method = method, variance = variance)
  out$aux$quantile <- NULL
  class(out) <- c("scf_median", "scf_percentile")
  out
}


#' @export
print.scf_median <- function(x, ...) {
  dollar_label <- if (isTRUE(attr(x, "nominal")))
    sprintf(" (nominal %d dollars)", x$aux$year) else ""
  cat(sprintf("Multiply-Imputed Median Estimate%s\n\n", dollar_label))
  print(.scf_shown_results(x$results), row.names = FALSE, ...)
  if (is.null(x$imps)) {
    cat("\nStack method: one weighted percentile from the stacked implicates; no standard error.\n")
  } else if (isTRUE(x$verbose)) {
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
  print(format(.scf_shown_results(object$results), digits = 4, nsmall = 2),
        row.names = FALSE, ...)
  if (is.null(object$imps)) {
    cat("\nStack method: one weighted percentile from the stacked implicates; no standard error.\n")
  } else {
    cat("\nImplicate-Level Estimates:\n")
    imp_df <- do.call(rbind, object$imps)
    print(format(imp_df, digits = 4, nsmall = 2), row.names = FALSE)
  }
  invisible(object)
}
