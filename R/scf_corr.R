#' Estimate Correlation Between Two Continuous Variables in SCF Microdata
#'
#' Computes the Pearson correlation coefficient between two continuous variables using
#' multiply-imputed, replicate-weighted SCF data. Returns pooled estimates and standard errors
#' using Rubin’s Rules.
#'
#' @description
#' This function estimates the linear association between two continuous variables
#' using Pearson's correlation. Estimates are computed within each implicate and then
#' pooled across implicates to account for imputation uncertainty.
#'
#' @section Implementation:
#' - Inputs: an `scf_mi_survey` object and two one-sided formulas (e.g., `~income`)
#' - Correlation computed using `cor(..., use = "complete.obs")` within each implicate
#' - Rubin’s Rules applied to pool results across implicates
#'
#' @section Interpretation:
#' Pearson’s `$r$` ranges from -1 to +1 and reflects the strength and
#' direction of a linear bivariate association between two continuous variables.
#' Values near 0 indicate weak linear association. Note that the operation is
#' sensitive to outliers and does not capture nonlinear relationships nor adjust
#' for covariates.
#'
#' @param scf An `scf_mi_survey` object, created by [scf_load()]
#' @param var1 One-sided formula specifying the first variable
#' @param var2 One-sided formula specifying the second variable
#'
#'
#' @section Statistical Notes:
#' Correlation is computed within each implicate using complete cases. Rubin’s
#' Rules are applied manually to pool estimates and calculate total variance.
#' Degrees of freedom are adjusted using the Barnard-Rubin method.
#' This function does not use [scf_MIcombine()], which is intended
#' for vector-valued estimates; direct pooling is more appropriate for
#' scalar statistics like correlation coefficients.
#'
#' @seealso [scf_plot_hex()], [scf_ols()]
#'
#' @return An object of class `scf_corr`, containing: 
#' \describe{
#'   \item{results}{Data frame with pooled correlation estimate, standard error, 
#'     t-statistic, degrees of freedom, p-value, and minimum/maximum values across implicates.}
#'   \item{imps}{Named vector of implicate-level correlations.}
#'   \item{aux}{Variable names used in the estimation.}
#' }
#'
#' @note Degrees of freedom are approximated using a simplified Barnard–Rubin 
#' adjustment, since correlation is a scalar quantity. Interpret cautiously with 
#' few implicates.
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td  <- tempdir()
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Estimate correlation between income and net worth
#' corr <- scf_corr(scf2022, ~income, ~networth)
#' print(corr)
#' summary(corr)
#' 
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink("scf2022.rds", force = TRUE)
#' 
#' @export
scf_corr <- function(scf, var1, var2) {
  if (isTRUE(attr(scf, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }

  if (!inherits(scf, "scf_mi_survey") ||
      !is.list(scf$mi_design) ||
      !all(sapply(scf$mi_design, inherits, "svyrep.design"))) {
    stop("Input must be an 'scf_mi_survey' object with valid replicate-weighted designs.")
  }

  v1 <- all.vars(var1)[1]
  v2 <- all.vars(var2)[1]
  designs <- scf$mi_design
  nimp <- length(designs)

  cors <- sapply(seq_along(designs), function(i) {
    d <- designs[[i]]
    dvars <- d$variables
    stats::cor(dvars[[v1]], dvars[[v2]], use = "complete.obs")
  })
  names(cors) <- paste0("imp", seq_len(nimp))

  qbar <- mean(cors)
  b <- var(cors)
  se <- sqrt(b * (1 + 1/nimp))

  # Rubin degrees of freedom
  r <- (1 + 1/nimp) * b / (b / nimp)
  df <- (nimp - 1) * (1 + 1/r)^2
  tval <- qbar / se
  pval <- 2 * stats::pt(-abs(tval), df = df)

  out <- list(
    results = data.frame(
      correlation = qbar,
      se = se,
      t = tval,
      df = df,
      p.value = pval,
      min = min(cors),
      max = max(cors),
      stringsAsFactors = FALSE
    ),
    imps = cors,
    aux = list(var1 = v1, var2 = v2)
  )
  class(out) <- "scf_corr"
  return(out)
}

#' @export
print.scf_corr <- function(x, ...) {
  cat("SCF Correlation Estimate\n")
  cat(sprintf("Variables: %s and %s\n\n", x$aux$var1, x$aux$var2))
  print(x$results, row.names = FALSE)
  invisible(x)
}

#' @export
summary.scf_corr <- function(object, ...) {
  cat("Summary of SCF Correlation Analysis\n")
  cat("Variables:", object$aux$var1, "and", object$aux$var2, "\n\n")
  cat("Pooled Correlation Estimate:\n")
  print(object$results, row.names = FALSE)
  cat("\nImplicate-level Correlations:\n")
  print(data.frame(
    implicate = names(object$imps),
    correlation = object$imps,
    row.names = NULL
  ))
  invisible(object)
}
