#' Estimate Correlation Between Two Continuous Variables in SCF Microdata
#'
#' Estimates the weighted Pearson correlation between two variables, with a
#' t-test of zero correlation.
#'
#' @details
#' Use it to measure how closely two variables move together, such as income
#' and net worth. \eqn{r} runs from -1 to 1: the sign gives the direction,
#' values near 0 mean little linear association. A small p-value means the
#' population correlation is unlikely to be zero. \eqn{r} measures linear
#' association only, is sensitive to outliers, and does not adjust for other
#' variables; for that, use [scf_ols()].
#'
#' In each implicate, the weighted correlation is
#'
#' \deqn{r = \frac{\sum w_i (x_i - \bar{x})(y_i - \bar{y})}{\sqrt{\sum w_i (x_i - \bar{x})^2 \sum w_i (y_i - \bar{y})^2}}}{r = sum(w (x - xbar)(y - ybar)) / sqrt(sum(w (x - xbar)^2) sum(w (y - ybar)^2))}
#'
#' where \eqn{w_i} is the household weight and \eqn{\bar{x}}, \eqn{\bar{y}}
#' are weighted means. The sampling variance comes from the 999 replicate
#' weights (see [scf_variance]). The five estimates are pooled (see
#' [scf_MIcombine]). The t-statistic is \eqn{r} divided by its standard error.
#'
#' @param scf A `scf_mi_survey` object, created by [scf_load()]
#' @param var1 One-sided formula specifying the first variable
#' @param var2 One-sided formula specifying the second variable
#' @param variance Variance method: `"fed"` (default) or `"rubin"`. See [scf_variance].
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
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("corr_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Correlate income and net worth
#' corr <- scf_corr(scf2022, ~income, ~networth)
#' print(corr)
#' summary(corr)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @export
scf_corr <- function(scf, var1, var2, variance = getOption("scf.variance", "fed")) {
  if (!inherits(scf, "scf_mi_survey") ||
      !is.list(scf$mi_design) ||
      !all(sapply(scf$mi_design, inherits, "svyrep.design"))) {
    stop("Input must be an 'scf_mi_survey' object with valid replicate-weighted designs.")
  }
  variance <- .scf_variance_method(variance)

  v1 <- deparse(var1[[2]])
  v2 <- deparse(var2[[2]])
  .scf_check_na(scf$mi_design, var1, var2)
  designs <- scf$mi_design
  nimp <- length(designs)

  wcor <- function(x, y, w) {
    ok <- !is.na(x) & !is.na(y) & !is.na(w)
    x <- x[ok]
    y <- y[ok]
    w <- w[ok]
    mx <- sum(w * x) / sum(w)
    my <- sum(w * y) / sum(w)
    sum(w * (x - mx) * (y - my)) /
      sqrt(sum(w * (x - mx)^2) * sum(w * (y - my)^2))
  }

  est <- lapply(designs, function(d) {
    rep_est <- survey::withReplicates(d, theta = function(w, data) {
      wcor(.scf_eval(var1, data), .scf_eval(var2, data), w)
    })
    c(cor = unname(coef(rep_est)), var = unname(as.matrix(stats::vcov(rep_est))[1, 1]))
  })
  cors <- vapply(est, function(e) e[["cor"]], numeric(1))
  vars <- vapply(est, function(e) e[["var"]], numeric(1))
  names(cors) <- paste0("imp", seq_len(nimp))

  pooled <- .scf_pool(cors, vars, variance)
  qbar <- pooled$estimate
  se <- pooled$se
  df <- pooled$df
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

#' @rdname scf_corr
#' @param object A `scf_corr` object returned by [scf_corr()].
#' @param ... Currently unused; included for S3 generic compatibility.
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
