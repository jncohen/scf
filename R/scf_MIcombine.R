#' Combine Estimates Across SCF Implicates Using Rubin's Rules
#'
#' This function implements **Rubin’s Rules** for combining multiply-imputed
#' survey model results in the `scf` package. It pools point estimates,
#' variance-covariance matrices, and degrees of freedom across the SCF’s
#' five implicates.
#' 
#' @section Scope:
#' `scf_MIcombine()` is used for **model-based analyses** such as
#' [scf_ols()], [scf_glm()], and [scf_logit()], where each implicate’s model
#' output includes both parameter estimates and replicate-weighted sampling
#' variances.
#'
#' Descriptive estimators—functions such as [scf_mean()], [scf_percentile()],
#' and [scf_median()]—do **not** apply Rubin’s Rules. They follow the Survey of
#' Consumer Finances convention used in the Federal Reserve Board’s SAS macro,
#' combining (i) the replicate-weight sampling variance from implicate 1 with
#' (ii) the between-implicate variance scaled by (m + 1)/m.
#' 
#' This separation is intentional: descriptive statistics in `scf` aim to
#' reproduce the Survey of Consumer Finances' published standard errors,
#' whereas model-based functions use Rubin's Rules.
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
#' Inputs are typically produced by modeling functions such as [scf_ols()], 
#' [scf_glm()], or [scf_logit()], which return implicate-level coefficient 
#' vectors and variance-covariance matrices.
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
#' @param results A list of implicate-level model outputs. Each element must be a named numeric vector
#' or an object with methods for `coef()` and `vcov()`. Typically generated internally by modeling functions.
#' @param variances Optional list of variance-covariance matrices. If omitted, extracted using `vcov()`.
#' @param call Optional. The originating function call. Defaults to `sys.call()`.
#' @param df.complete Optional degrees of freedom for the complete-data model. Used for small-sample
#' corrections. Defaults to `Inf`, assuming large-sample asymptotics.
#'
#' @return An object of class `"scf_MIresult"` with components:
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
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td  <- tempdir()
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Pool simple survey mean for mock data
#' outlist <- lapply(scf2022$mi_design, function(d) survey::svymean(~I(age >= 65), d))
#' pooled  <- scf_MIcombine(outlist)     # vcov/coef extracted automatically
#' SE(pooled); coef(pooled)
#' 
#' unlink(file.path(td, "scf2022.rds"), force = TRUE)
#'
#' @references
#'
#' Barnard J, Rubin DB. Small-sample degrees of freedom with multiple imputation.
#'   \doi{10.1093/biomet/86.4.948}.
#'   
#' Little RJA, Rubin DB. Statistical analysis with missing data.
#'   ISBN: 9780470526798.
#'   
#' U.S. Federal Reserve. Codebook for 2022 Survey of Consumer Finances.
#'   https://www.federalreserve.gov/econres/scfindex.htm
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

#' Extract Standard Errors from Pooled SCF Model Results
#'
#' @description
#' Generic extractor for pooled standard errors from objects of class
#' `"scf_MIresult"`.
#'
#' @param object A pooled result object of class `"scf_MIresult"`.
#' @param ... Not used.
#'
#' @return A numeric vector of standard errors.
#'
#' @seealso scf_MIcombine
#' @export
SE <- function(object, ...) UseMethod("SE")

#' @rdname SE
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

