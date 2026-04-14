#' Estimate a Quantile Regression Model on SCF Microdata
#'
#' Fits a survey-weighted quantile regression to each implicate of
#' multiply-imputed SCF data and pools coefficients and standard errors using
#' Rubin's Rules.
#'
#' @description
#' `scf_quantreg()` estimates a linear quantile regression at a user-specified
#' quantile (`tau`) across the five SCF implicates. Each implicate is fit
#' independently via [quantreg::rq()] using the SCF final sampling weights.
#' Coefficient estimates and variance-covariance matrices are then pooled across
#' implicates using [scf_MIcombine()].
#'
#' Unlike [scf_ols()], which models the conditional mean, quantile regression
#' models the conditional quantile of the outcome distribution. This makes it
#' especially useful for analyzing wealth and income data, which are highly
#' right-skewed: the conditional median (`tau = 0.5`) and upper quantiles
#' (`tau = 0.75`, `tau = 0.90`) describe the distribution more completely than
#' the mean alone.
#'
#' @section Standard Error Methods:
#'
#' The `se` argument controls how within-implicate variance is estimated.
#' All methods produce a variance-covariance matrix passed to [scf_MIcombine()].
#'
#' \describe{
#'   \item{`"nid"` (default)}{Non-iid sandwich estimator. Allows the
#'     conditional sparsity (density of the error distribution at the quantile)
#'     to vary across observations. Appropriate when the shape of the error
#'     distribution differs across the covariate space, which is typical for
#'     skewed outcomes such as wealth and income.}
#'   \item{`"iid"`}{Assumes identically distributed errors, i.e., constant
#'     sparsity across all observations. Implements the covariance formula from
#'     Koenker and Bassett (1978, Theorem 4.2): \eqn{[\theta(1-\theta)/f(\xi(\theta))^2] Q^{-1}},
#'     where \eqn{f(\xi(\theta))} is the density of the error distribution at its
#'     \eqn{\theta}-quantile and \eqn{Q = \lim T^{-1}X'X}. The sparsity is
#'     estimated from the residual distribution. Fastest option; appropriate when
#'     the error distribution does not vary across observations.}
#'   \item{`"ker"`}{Kernel smoothing estimate of the conditional sparsity.
#'     More data-adaptive than `"nid"` but slower. Suitable for large samples.}
#'   \item{`"boot"`}{Pairs bootstrap over observations. Provides
#'     distribution-free variance estimates but is the slowest analytical
#'     option. Useful for robustness checks.}
#'   \item{`"replicate"`}{Replication-based variance estimation. For each
#'     implicate, the model is re-fit using each of the SCF's 999 replicate
#'     weight vectors. Variance is accumulated as the weighted sum of squared
#'     deviations from the full-weight estimate, matching the SCF's own
#'     published variance methodology. This is the most methodologically
#'     rigorous option, but is computationally intensive
#'     (~5,000 model fits for five implicates). Recommended for final
#'     publication-quality estimates.}
#' }
#'
#' @section Implementation:
#' For each implicate `m`:
#' \enumerate{
#'   \item Extracts the survey data and final sampling weights from the
#'     `svyrep.design` object.
#'   \item Fits [quantreg::rq()] at quantile `tau` with the final weights.
#'   \item Estimates the within-implicate variance-covariance matrix using
#'     the method specified by `se`.
#' }
#' Pooled estimates follow Rubin's Rules (see [scf_MIcombine()]):
#' the total variance combines within-implicate sampling uncertainty
#' and between-implicate imputation uncertainty.
#'
#' @param object A `scf_mi_survey` object created with [scf_load()] or
#'   [scf_design()].
#' @param formula A model formula specifying the outcome and predictors,
#'   e.g., `networth ~ age + factor(edcl)`. The outcome should be numeric.
#' @param tau Numeric scalar in (0, 1) specifying the quantile to estimate.
#'   Defaults to `0.5` (median regression). To estimate multiple quantiles,
#'   call `scf_quantreg()` separately for each.
#' @param se Character string specifying the standard error estimation method
#'   for within-implicate variance. One of `"nid"` (default), `"iid"`,
#'   `"ker"`, `"boot"`, or `"replicate"`. See the **Standard Error Methods**
#'   section for details.
#' @param ... Additional arguments passed to [quantreg::rq()].
#'
#' @return An object of class `"scf_quantreg"` and `"scf_model_result"` with:
#' \describe{
#'   \item{`results`}{A data frame of pooled coefficients, standard errors,
#'     t-values, p-values, and significance stars.}
#'   \item{`tau`}{The quantile estimated.}
#'   \item{`se_method`}{The SE method used.}
#'   \item{`fit`}{An empty list (AIC is not defined for quantile regression).}
#'   \item{`models`}{A list of implicate-level `rq` model objects for
#'     direct inspection.}
#'   \item{`call`}{The matched call.}
#'   \item{`formula`}{The model formula.}
#' }
#'
#' @examples
#' \donttest{
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("qreg_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Median regression of net worth on age and education
#' m_med <- scf_quantreg(scf2022, networth ~ age + factor(edcl), tau = 0.5)
#' print(m_med)
#'
#' # 75th-percentile regression
#' m_75 <- scf_quantreg(scf2022, networth ~ age + factor(edcl), tau = 0.75)
#' summary(m_75)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#' }
#'
#' @seealso [scf_ols()], [scf_glm()], [scf_MIcombine()], [quantreg::rq()]
#'
#' @references
#' Koenker R, Bassett G. Regression quantiles. \emph{Econometrica}. 1978;46(1):33--50. \doi{10.2307/1913643}
#'
#' Koenker R. \emph{Quantile Regression}. Cambridge University Press; 2005. \doi{10.1017/CBO9780511754098}
#'
#' @importFrom stats weights pt coef vcov
#' @importFrom survey withReplicates
#' @export
scf_quantreg <- function(object, formula, tau = 0.5,
                          se = c("nid", "iid", "ker", "boot", "replicate"),
                          ...) {

  # ---- Input validation -----------------------------------------------------
  if (!inherits(object, "scf_mi_survey"))
    stop("Input must be of class 'scf_mi_survey'.")
  if (!inherits(formula, "formula"))
    stop("'formula' must be a formula object (e.g., networth ~ age + income).")
  if (!requireNamespace("quantreg", quietly = TRUE))
    stop("Package 'quantreg' is required. Install with: install.packages('quantreg')")
  if (!is.numeric(tau) || length(tau) != 1 || tau <= 0 || tau >= 1)
    stop("'tau' must be a single numeric value strictly between 0 and 1.")

  se <- match.arg(se)

  if (isTRUE(attr(object, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.",
            call. = FALSE)
  }

  # ---- Fit implicate-level models -------------------------------------------
  models <- lapply(object$mi_design, function(imp) {
    df  <- imp$variables
    wts <- stats::weights(imp, type = "sampling")
    tryCatch(
      quantreg::rq(formula, tau = tau, data = df, weights = wts, ...),
      error = function(e) NULL
    )
  })

  models <- Filter(Negate(is.null), models)
  if (length(models) < 2)
    stop("Too few successful implicate-level models to pool (need >= 2).")

  coefs_list <- lapply(models, stats::coef)

  # ---- Estimate within-implicate variance -----------------------------------
  if (se == "replicate") {
    # Full replication-based variance: re-fit with each of the 999 replicate
    # weight vectors and accumulate weighted squared deviations.
    vars_list <- Map(
      function(m, imp) .scf_rq_repvar(m, imp, formula, tau),
      models,
      object$mi_design[seq_along(models)]
    )
  } else {
    vars_list <- lapply(models, function(m) .scf_rq_vcov(m, se))
  }

  # ---- Align terms across implicates ----------------------------------------
  common_terms <- Reduce(intersect, lapply(coefs_list, names))
  if (length(common_terms) == 0)
    stop("No common coefficient terms found across implicates.")

  coefs_list <- lapply(coefs_list, function(x) x[common_terms])
  vars_list  <- lapply(vars_list,  function(v) {
    v[common_terms, common_terms, drop = FALSE]
  })

  # ---- Pool via Rubin's Rules -----------------------------------------------
  pooled <- scf_MIcombine(coefs_list, vars_list)

  est   <- stats::coef(pooled)
  se_v  <- SE(pooled)
  tval  <- est / se_v
  pval  <- 2 * stats::pt(-abs(tval), df = pooled$df)

  coef_table <- data.frame(
    term      = names(est),
    estimate  = unname(est),
    std.error = unname(se_v),
    t.value   = unname(tval),
    p.value   = unname(pval),
    stars     = cut(pval,
                    breaks = c(-Inf, 0.001, 0.01, 0.05, 0.10, Inf),
                    labels = c("***", "**", "*", "^", ""),
                    right  = FALSE),
    stringsAsFactors = FALSE
  )

  out <- list(
    results   = coef_table,
    tau       = tau,
    se_method = se,
    fit       = list(),          # AIC not defined for quantile regression
    models    = models,
    call      = match.call(),
    formula   = formula
  )
  class(out) <- c("scf_quantreg", "scf_model_result")
  return(out)
}


# ---- Internal: analytical variance from quantreg::summary.rq ----------------

#' @keywords internal
.scf_rq_vcov <- function(m, se_method) {
  # summary.rq with cov = TRUE returns $cov (p x p matrix) for most se methods.
  s <- tryCatch(
    suppressWarnings(summary(m, se = se_method, cov = TRUE)),
    error = function(e) {
      tryCatch(
        suppressWarnings(summary(m, se = "iid", cov = TRUE)),
        error = function(e2) NULL
      )
    }
  )

  # Primary: use $cov if present
  if (!is.null(s) && !is.null(s$cov)) {
    cov_mat <- s$cov
    nm <- names(stats::coef(m))
    dimnames(cov_mat) <- list(nm, nm)
    return(cov_mat)
  }

  # Fallback: diagonal matrix from the SE column of the coefficient table
  if (!is.null(s) && !is.null(s$coefficients)) {
    se_vec <- s$coefficients[, "Std. Error"]
    cov_mat <- diag(se_vec^2, nrow = length(se_vec))
    nm <- rownames(s$coefficients)
    dimnames(cov_mat) <- list(nm, nm)
    return(cov_mat)
  }

  # Last resort: near-zero diagonal (signals near-perfect fit or degenerate data)
  p  <- length(stats::coef(m))
  nm <- names(stats::coef(m))
  cov_mat <- diag(rep(1e-10, p))
  dimnames(cov_mat) <- list(nm, nm)
  warning("Could not compute variance-covariance matrix for one implicate. ",
          "Using near-zero diagonal as placeholder.", call. = FALSE)
  cov_mat
}


# ---- Internal: replication-based variance via survey::withReplicates ----------

#' @keywords internal
.scf_rq_repvar <- function(m, imp, formula, tau) {
  # Use survey::withReplicates() — Lumley's canonical API for computing
  # design-based variance for arbitrary statistics from replicate-weight
  # designs. It applies theta() to the full-weight design and to each of the
  # 999 replicate weight vectors, then accumulates the weighted squared
  # deviations using the design's own scale and rscales. This correctly
  # propagates the SCF's replication scheme without reimplementing it.
  #
  # Note: quantile regression point estimates come from quantreg::rq() with
  # the final sampling weights (survey has no svyquantile-based regression
  # function). withReplicates() provides the design-based variance wrapper
  # around that estimator.

  nm <- names(stats::coef(m))
  p  <- length(nm)
  df <- imp$variables  # captured in closure; avoids passing data as argument

  theta_fn <- function(w, ...) {
    fit <- tryCatch(
      suppressWarnings(
        quantreg::rq(formula, tau = tau, data = df, weights = w)
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) return(rep(NA_real_, p))
    coef_r <- stats::coef(fit)
    # Align to the expected term order; fill NA for any missing terms
    out <- rep(NA_real_, p)
    names(out) <- nm
    matched <- intersect(names(coef_r), nm)
    out[matched] <- coef_r[matched]
    out
  }

  rep_result <- survey::withReplicates(imp, theta = theta_fn)
  stats::vcov(rep_result)
}


# ---- S3 methods: print and summary -------------------------------------------

#' @export
#' @method print scf_quantreg
print.scf_quantreg <- function(x, digits = 4, ...) {
  cat(sprintf("Quantile Regression Results (tau = %.2f, Multiply-Imputed SCF)\n", x$tau))
  cat("------------------------------------------------------------------\n")

  df <- x$results
  df$estimate   <- round(df$estimate,   digits)
  df$std.error  <- round(df$std.error,  digits)
  df$t.value    <- round(df$t.value,    digits)
  df$p.value    <- format.pval(df$p.value, digits = digits)

  print(df[, c("term", "estimate", "std.error", "t.value", "p.value", "stars")],
        row.names = FALSE)

  cat(sprintf("\nSE method: %s | Implicates pooled: %d\n",
              x$se_method, length(x$models)))
  cat("Implicate-level rq objects stored in `object$models`.\n")
  invisible(x)
}

#' @export
#' @method summary scf_quantreg
summary.scf_quantreg <- function(object, digits = 4, ...) {
  cat(sprintf("SCF Quantile Regression Summary (tau = %.2f)\n", object$tau))
  cat("------------------------------------------------------------------\n")

  df <- object$results
  df$estimate   <- round(df$estimate,   digits)
  df$std.error  <- round(df$std.error,  digits)
  df$t.value    <- round(df$t.value,    digits)
  df$p.value    <- format.pval(df$p.value, digits = digits)

  cat("Pooled Coefficient Estimates:\n")
  print(df[, c("term", "estimate", "std.error", "t.value", "p.value", "stars")],
        row.names = FALSE)

  cat(sprintf(
    "\nQuantile:        %.2f\nSE method:       %s\nImplicates used: %d\n",
    object$tau, object$se_method, length(object$models)
  ))

  cat("\nCall:\n")
  print(object$call)
  invisible(object)
}
