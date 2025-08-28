#' Combine Estimates Across SCF Implicates Using Rubin's Rules
#'
#' This function is the **canonical implementation** of Rubin’s Rules in the
#' `scf` package. It defines how point estimates, standard errors, and degrees
#' of freedom are pooled across the SCF’s multiply-imputed replicate-weighted
#' implicates.
#'
#' Most `scf` functions that compute descriptive statistics or model estimates
#' internally call this function (or apply its logic). As such, this
#' documentation serves as the authoritative explanation of the pooling
#' procedure and its assumptions for all SCF workflows in the package.
#'
#' @section Implementation: 
#' `scf_MIcombine()` pools a set of implicate-level estimates and their
#' associated variance-covariance matrices using Rubin’s Rules.
#'
#' This includes:
#' - Calculation of pooled point estimates
#' - Total variance from within- and between-imputation components
#' - Degrees of freedom via Barnard-Rubin method
#' - Fraction of missing information
#'
#' Inputs are typically produced by functions like `scf_mean()`, `scf_ols()`,
#' or `scf_percentile()`.
#'
#' This function is primarily used internally, but may be called directly by
#' advanced users constructing custom estimation routines from implicate-level
#' results.
#'
#' @section Details:
#' The SCF provides five implicates per survey wave, each a plausible version
#' of the population under a specific missing-data model. Analysts conduct the
#' same statistical procedure on each implicate, producing a set of five
#' estimates \eqn{ Q_1, Q_2, ..., Q_5 }. These are then combined using Rubin’s
#' Rules, a procedure to combine results across these implicates with an
#' attempt to account for:
#'
#' - **Within-imputation variance**: Uncertainty from complex sample design
#' - **Between-imputation variance**: Uncertainty due to missing data
#'
#'For a scalar quantity \eqn{ Q }, the pooled estimate and
#' total variance are calculated as:
#'
#' \deqn{ \bar{Q} = \frac{1}{M} \sum Q_m }
#' \deqn{ \bar{U} = \frac{1}{M} \sum U_m }
#' \deqn{ B = \frac{1}{M - 1} \sum (Q_m - \bar{Q})^2 }
#' \deqn{ T = \bar{U} + \left(1 + \frac{1}{M} \right) B }
#'
#' Where:
#' - \eqn{ M } is the number of implicates (typically 5 for SCF)
#' - \eqn{ Q_m } is the estimate from implicate \eqn{ m }
#' - \eqn{ U_m } is the sampling variance of \eqn{ Q_m }, accounting for replicate weights and design
#'
#' The total variance \eqn{ T } reflects both within-imputation uncertainty (sampling error)
#' and between-imputation uncertainty (missing-data imputation).
#'
#' The standard error of the pooled estimate is \eqn{ \sqrt{T} }. Degrees of freedom are
#' adjusted using the Barnard-Rubin method:
#'
#' \deqn{ \nu = (M - 1) \left(1 + \frac{\bar{U}}{(1 + \frac{1}{M}) B} \right)^2 }
#'
#' The fraction of missing information (FMI) is also reported:
#' it reflects the proportion of total variance attributable to imputation uncertainty.
#'
#' See [scf_MIcombine()] for full implementation details.
#'
#' @param results A list of implicate-level model outputs. Each element must be a named numeric vector
#' or an object with methods for `coef()` and `vcov()`. Typically generated internally by modeling functions.
#' @param variances Optional list of variance-covariance matrices. If omitted, extracted using `vcov()`.
#' @param call Optional. The originating function call. Defaults to `sys.call()`.
#' @param df.complete Optional degrees of freedom for the complete-data model. Used for small-sample
#' corrections. Defaults to `Inf`, assuming large-sample asymptotics.
#'
#' @return An object of class `"MIresult"` with components:
#' \describe{
#'   \item{coefficients}{Pooled point estimates across implicates.}
#'   \item{variance}{Pooled variance-covariance matrix.}
#'   \item{df}{Degrees of freedom for each parameter, adjusted using Barnard-Rubin formula.}
#'   \item{missinfo}{Estimated fraction of missing information for each parameter.}
#'   \item{nimp}{Number of implicates used in pooling.}
#'   \item{call}{Function call recorded for reproducibility.}
#' }
#'
#' Supports `coef()`, [SE()], `confint()`, and `summary()` methods.
#'
#' @seealso
#' [scf_mean()], [scf_ols()], [scf_glm()], [scf_logit()]
#'
#' @examples
#' \dontrun{
#' # Not for general use — shown for internal method developers only:
#' design <- scf_load(2022)
#' outlist <- lapply(design$mi_design, function(d) survey::svymean(~I(age >= 65), d))
#' pooled <- scf_MIcombine(outlist)
#' summary(pooled)
#' }
#'
#' @references
#'
#' Barnard, John and Donald B. Rubin (1999) "Miscellanea. Small-Sample Degrees of Freedom with Multiple Imputation" *Biometrika*
#'
#' Cohen, Joseph N. (2025) "The `scf` Package: Analyzing the Survey of Consumer Finances in R". Manuscript in Review.
#'
#' Little, Roderick J. A. and Donald B. Rubin (1987) *Statistical Analysis with Missing Data.* Wiley.
#'
#' U.S. Federal Reserve (2023) *Codebook for 2022 Survey of Consumer Finances* <https://www.federalreserve.gov/econres/files/codebk2022.txt>
#'
#' @export
scf_MIcombine <- function(results, variances, call = sys.call(), df.complete = Inf) {
  m <- length(results)
  if (missing(variances)) {
    variances <- suppressWarnings(lapply(results, vcov))
    results <- lapply(results, coef)
  }

  vbar <- Reduce("+", variances) / m
  cbar <- Reduce("+", results) / m
  evar <- var(do.call("rbind", results))

  r <- (1 + 1/m) * evar / vbar
  df <- (m - 1) * (1 + 1/r)^2
  if (is.matrix(df)) df <- diag(df)

  if (is.finite(df.complete)) {
    dfobs <- ((df.complete + 1)/(df.complete + 3)) * df.complete * vbar / (vbar + evar)
    if (is.matrix(dfobs)) dfobs <- diag(dfobs)
    df <- 1 / (1/dfobs + 1/df)
  }

  missinfo <- (r + 2/(df + 3)) / (r + 1)

  structure(
    list(
      coefficients = cbar,
      variance = vbar + (1 + 1/m) * evar,
      call = call,
      nimp = m,
      df = df,
      missinfo = missinfo
    ),
    class = "scf_MIresult"
  )
}

#' @export
SE <- function(object, ...) UseMethod("SE")

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

