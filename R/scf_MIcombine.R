#' Combine Estimates Across SCF Implicates
#'
#' @description
#' The SCF comes as five implicates: five complete copies of the data that
#' differ only in the values the Fed filled in for missing answers. Every
#' `scf` estimate is computed on each implicate and the five results are
#' combined into a consolidated, final result. This page explains how these
#' results are produced. To see how variance is computed, see [scf_variance].
#'
#' `scf_MIcombine()` does the combining for the regression models. It can
#' also be called directly on any list of implicate results. For univariate
#' and bivariate inferential statistics, the results are combined within the
#' functions that compute them, such as [scf_mean()], [scf_ratio()], as these
#' operations can differ depending on the statistic. Call it yourself when you
#' need a statistic or model that `scf` does not provide: compute it on each
#' implicate and pass the list of results.
#'
#' @section The estimate:
#' The final estimate is the average of the implicate estimates:
#'
#' \deqn{\bar{Q} = \frac{1}{m} \sum_{i=1}^{m} Q_i}{Qbar = (1/m) sum(Q_i)}
#'
#' where \eqn{Q_i} is the estimate from implicate \eqn{i} and \eqn{m} is the
#' number of implicates (5).
#'
#' @section The standard error:
#' The total variance adds two parts:
#'
#' \deqn{T = U + \left(1 + \frac{1}{m}\right) B}{T = U + (1 + 1/m) B}
#'
#' \eqn{U} is the sampling variance, from the replicate weights (see
#' [scf_variance]). \eqn{B} is the variance of the \eqn{m} implicate
#' estimates around \eqn{\bar{Q}}, which measures how much the filled-in
#' values move the answer. The factor \eqn{1 + 1/m} allows for having only
#' five implicates. The standard error is \eqn{\sqrt{T}}. The Fed's codebook
#' uses the same formula, written as \eqn{U + (6/5) B}.
#'
#' @section Tests and confidence intervals:
#' Tests and intervals use a t distribution. Its degrees of freedom are:
#'
#' \deqn{\nu = (m - 1) \left(1 + \frac{U}{(1 + 1/m) B}\right)^2}{nu = (m - 1) (1 + U / ((1 + 1/m) B))^2}
#'
#' It depends on the ratio of sampling variance (\eqn{U}) to imputation
#' variance (\eqn{B}). When imputation adds little variance, \eqn{\nu} is
#' large and the t is close to normal. When it adds a lot, \eqn{\nu} can be
#' small. If `df.complete` is given, the Barnard-Rubin small-sample version
#' is used instead.
#'
#' The fraction of missing information (`missinfo`) is the share of the
#' total variance that comes from the missing data. Values near 0 mean the
#' imputation adds little uncertainty. Large values mean the result leans
#' heavily on imputed answers.
#'
#' @section Regression models:
#' [scf_ols()], [scf_glm()], [scf_logit()], and [scf_quantreg()] fit the
#' model separately on each implicate, then combine the results the same way,
#' with vectors and matrices in place of single numbers:
#'
#' - The coefficients are the average of the implicate coefficients.
#' - \eqn{U} is the average of the implicate variance-covariance matrices.
#' - \eqn{B} is the covariance matrix of the coefficients across implicates.
#' - \eqn{T = U + (1 + 1/m) B}, and each coefficient gets its own \eqn{\nu}.
#'
#' This is what the Fed's codebook does for regressions (the MISECOMP SAS
#' macro). Models always average \eqn{U} over all implicates.
#'
#' Fit statistics, such as R-squared, are summarized across implicates
#' rather than combined with Rubin's rules. Each model's help page says how.
#'
#' @section Groups missing from some implicates:
#' A group can be empty in some implicates, for example when a subset uses an
#' imputed variable. Its estimate then combines only the implicates that have
#' it, and \eqn{m} is the number of those implicates.
#'
#' @param results A list of implicate-level results. Each element is an object with `coef()` and
#' `vcov()` methods, or a named numeric vector when `variances` is given.
#' @param variances Optional list of variance-covariance matrices. If omitted, extracted using `vcov()`.
#' @param call Optional. The originating function call. Defaults to `sys.call()`.
#' @param df.complete Optional degrees of freedom for the complete-data model. Used for small-sample
#' corrections. Defaults to `Inf`, assuming large-sample asymptotics.
#' @param object A pooled result object of class `"scf_MIresult"` (for `SE()`).
#' @param ... Not used.
#'
#' @return An object of class `"scf_MIresult"` with components:
#' \describe{
#'   \item{coefficients}{Pooled point estimates across implicates.}
#'   \item{variance}{Pooled variance-covariance matrix.}
#'   \item{df}{Degrees of freedom for each parameter.}
#'   \item{missinfo}{Estimated fraction of missing information for each parameter.}
#'   \item{nimp}{Number of implicates used in pooling.}
#'   \item{call}{The function call.}
#' }
#'
#' Supports `coef()`, `vcov()`, `confint()` (t distribution with Rubin's
#' degrees of freedom), and [SE()].
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("MIcombine_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Pool simple survey mean for mock data
#' outlist <- lapply(scf2022$mi_design, function(d) survey::svymean(~I(age >= 65), d))
#' pooled  <- scf_MIcombine(outlist)     # vcov/coef extracted automatically
#' SE(pooled); coef(pooled)
#'
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @references
#'
#' Barnard J, Rubin DB. Small-sample degrees of freedom with multiple
#'   imputation. \emph{Biometrika}. 1999;86(4):948--955.
#'   \doi{10.1093/biomet/86.4.948}
#'
#' Board of Governors of the Federal Reserve System. Codebook for the 2022
#'   Survey of Consumer Finances.
#'   <https://www.federalreserve.gov/econres/scfindex.htm>
#'
#' Little RJA, Rubin DB. \emph{Statistical Analysis with Missing Data}.
#'   3rd ed. Wiley; 2019. ISBN: 9780470526798.
#'
#' Rubin DB. \emph{Multiple Imputation for Nonresponse in Surveys}. Wiley;
#'   1987.
#'
#' @export
scf_MIcombine <- function(results, variances, call = sys.call(), df.complete = Inf) {
  if (missing(variances)) {
    variances <- suppressWarnings(lapply(results, vcov))
    results <- lapply(results, coef)
  }
  m <- length(results)
  if (m < 2) stop("Need results from at least two implicates.")

  est <- do.call(rbind, results)
  if (anyNA(est) || anyNA(unlist(variances))) {
    stop("Some implicate results are missing (NA).")
  }

  qbar <- colMeans(est)
  ubar <- Reduce("+", variances) / m
  b <- stats::var(est)
  total <- ubar + (1 + 1/m) * b

  u <- diag(as.matrix(ubar))
  bm <- diag(b)
  r <- (1 + 1/m) * bm / u
  df <- (m - 1) * (1 + 1/r)^2

  if (is.finite(df.complete)) {
    df_obs <- (df.complete + 1) / (df.complete + 3) * df.complete * u / (u + bm)
    df <- 1 / (1/df_obs + 1/df)
  }

  missinfo <- (r + 2/(df + 3)) / (r + 1)

  structure(
    list(
      coefficients = qbar,
      variance = total,
      call = call,
      nimp = m,
      df = df,
      missinfo = missinfo
    ),
    class = "scf_MIresult"
  )
}

#' @importFrom survey SE
#' @export
survey::SE

#' @rdname scf_MIcombine
#' @export
SE.scf_MIresult <- function(object, ...) {
  sqrt(diag(object$variance))
}

#' @export
coef.scf_MIresult <- function(object, ...) {
  object$coefficients
}

#' @export
vcov.scf_MIresult <- function(object, ...) {
  object$variance
}

#' @export
confint.scf_MIresult <- function(object, parm, level = 0.95, ...) {
  est <- object$coefficients
  se <- sqrt(diag(as.matrix(object$variance)))
  q <- stats::qt(1 - (1 - level) / 2, object$df)
  lims <- cbind(est - q * se, est + q * se)
  pct <- paste(format(100 * c((1 - level) / 2, 1 - (1 - level) / 2), trim = TRUE), "%")
  dimnames(lims) <- list(names(est), pct)
  if (!missing(parm)) lims <- lims[parm, , drop = FALSE]
  lims
}

#' @export
print.scf_MIresult <- function(x, digits = 4, ...) {
  se <- sqrt(diag(as.matrix(x$variance)))
  tab <- data.frame(
    estimate = unname(x$coefficients),
    se = unname(se),
    df = unname(x$df),
    missinfo = unname(x$missinfo),
    row.names = names(x$coefficients)
  )
  cat("Multiple imputation results pooled across", x$nimp, "implicates\n\n")
  print(signif(tab, digits))
  invisible(x)
}
