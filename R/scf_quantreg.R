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
#'   \item{`"replicate"` (recommended)}{Replication-based variance estimation.
#'     For each implicate, the model is re-fit using each of the SCF's 999
#'     replicate weight vectors. Variance is accumulated as the weighted sum of
#'     squared deviations from the full-weight estimate, matching the SCF's own
#'     published variance methodology via [survey::withReplicates()].
#'     This method is theoretically preferred for quantile regression because
#'     replication-based variance estimators are consistent for sample quantiles,
#'     whereas analytical (sandwich) estimators are not guaranteed consistent for
#'     nonsmooth statistics (Rust and Rao, 1996).
#'     Computationally intensive (~5,000 model fits for five implicates).}
#'   \item{`"nid"` (default)}{Non-iid sandwich estimator. Allows the
#'     conditional sparsity (density of the error distribution at the quantile)
#'     to vary across observations. Appropriate when the shape of the error
#'     distribution differs across the covariate space, which is typical for
#'     skewed outcomes such as wealth and income. Unreliable near quantiles
#'     with high mass points (e.g., `tau <= 0.25` when net worth has substantial
#'     mass at zero); use `se = "replicate"` in such cases.}
#'   \item{`"iid"`}{Assumes identically distributed errors, i.e., constant
#'     sparsity across all observations. Implements the covariance formula from
#'     Koenker and Bassett (1978, Theorem 4.2): \eqn{[\theta(1-\theta)/f(\xi(\theta))^2] Q^{-1}},
#'     where \eqn{f(\xi(\theta))} is the density of the error distribution at its
#'     \eqn{\theta}-quantile and \eqn{Q = \lim T^{-1}X'X}. Fastest analytical
#'     option; subject to the same mass-point caveat as `"nid"`.}
#'   \item{`"ker"`}{Kernel smoothing estimate of the conditional sparsity.
#'     More data-adaptive than `"nid"` but slower. Suitable for large samples.}
#'   \item{`"boot"`}{Pairs bootstrap over observations. Provides
#'     distribution-free variance estimates but is the slowest analytical
#'     option. Useful for robustness checks.}
#' }
#'
#' @section Goodness of Fit:
#'
#' The `fit` component of the returned object contains per-quantile goodness-of-fit
#' measures following Koenker and Machado (1999). These are computed by comparing
#' the full model to an intercept-only null model at the same `tau`.
#'
#' \describe{
#'   \item{`rho`}{Mean minimized weighted sum of absolute residuals
#'     (\eqn{\sum \rho_\tau(y_i - x_i'\hat\beta)}) from the full model, averaged
#'     across implicates. This is the quantile regression analog of the residual
#'     sum of squares.}
#'   \item{`rho_null`}{Same quantity from the intercept-only null model. Larger
#'     values relative to `rho` indicate greater explanatory power.}
#'   \item{`r1`}{The Koenker-Machado R\eqn{^1(\tau)} statistic:
#'     \eqn{1 - \bar{V}(\tau) / \tilde{V}(\tau)}, where \eqn{\bar{V}} is the
#'     mean full-model objective and \eqn{\tilde{V}} is the mean null-model
#'     objective, each averaged across implicates. Ranges from 0 to 1. Measures
#'     the proportional reduction in the weighted sum of absolute residuals due
#'     to the covariates at quantile `tau`. This is a *local* measure for the
#'     specific quantile estimated; it is not a global summary of fit across
#'     the distribution.}
#'   \item{`r1_adj`}{Degrees-of-freedom-adjusted R\eqn{^1(\tau)}:
#'     \eqn{1 - (1 - R^1) \cdot n / (n - p)}, where \eqn{n} is the mean
#'     number of observations and \eqn{p} is the number of estimated parameters.
#'     Penalizes model complexity analogously to adjusted R\eqn{^2} in OLS.
#'     Note: this adjustment is not derived from the asymptotic theory in
#'     Koenker and Machado (1999) and should be interpreted descriptively.}
#'   \item{`nobs`}{Integer vector of per-implicate sample sizes.}
#'   \item{`nobs_mean`}{Mean sample size across successful implicates.}
#' }
#'
#' @section Implementation:
#' For each implicate:
#' \enumerate{
#'   \item Extracts the survey data and final sampling weights from the
#'     `svyrep.design` object.
#'   \item Fits [quantreg::rq()] at quantile `tau` with the final weights.
#'   \item Estimates the within-implicate variance-covariance matrix using
#'     the method specified by `se`.
#'   \item Fits an intercept-only [quantreg::rq()] at the same `tau` and
#'     weights to obtain the null-model objective value for goodness-of-fit.
#' }
#' SCF public-use microdata contain no missing values; each implicate is a
#' complete dataset. Pooled estimates follow Rubin's Rules (see
#' [scf_MIcombine()]): the total variance combines within-implicate sampling
#' uncertainty and between-implicate imputation uncertainty.
#'
#' @param object A `scf_mi_survey` object created with [scf_load()] or
#'   [scf_design()].
#' @param formula A model formula specifying the outcome and predictors,
#'   e.g., `networth ~ age + factor(edcl)`. The outcome should be numeric.
#' @param tau Numeric scalar in (0, 1) specifying the quantile to estimate.
#'   Defaults to `0.5` (median regression). To estimate multiple quantiles,
#'   call `scf_quantreg()` separately for each. Analytical SE methods (`"nid"`,
#'   `"iid"`, `"ker"`, `"boot"`) may be unreliable at quantiles with high mass
#'   points; prefer `se = "replicate"` in such cases.
#' @param se Character string specifying the standard error estimation method
#'   for within-implicate variance. One of `"nid"` (default), `"iid"`,
#'   `"ker"`, `"boot"`, or `"replicate"`. See the **Standard Error Methods**
#'   section for details. `"replicate"` is theoretically preferred for quantile
#'   regression but is computationally intensive.
#' @param ... Additional arguments passed to [quantreg::rq()].
#'
#' @return An object of class `"scf_quantreg"` and `"scf_model_result"` with:
#' \describe{
#'   \item{`results`}{A data frame of pooled coefficients, standard errors,
#'     t-values, p-values, and significance stars.}
#'   \item{`tau`}{The quantile estimated.}
#'   \item{`se_method`}{The SE method used.}
#'   \item{`fit`}{A list of goodness-of-fit statistics. See the
#'     **Goodness of Fit** section for details. Components: `rho`,
#'     `rho_null`, `r1`, `r1_adj`, `nobs`, `nobs_mean`.}
#'   \item{`models`}{A list of implicate-level `rq` model objects for
#'     direct inspection.}
#'   \item{`fit_errors`}{Character vector of any implicate-level errors.}
#'   \item{`call`}{The matched call.}
#'   \item{`formula`}{The model formula.}
#' }
#'
#' @examples
#' \dontrun{
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("qreg_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: median regression of net worth on age and education
#' m_med <- scf_quantreg(scf2022, networth ~ age + factor(edcl), tau = 0.5)
#' summary(m_med)
#'
#' # Access goodness-of-fit statistics
#' m_med$fit$r1
#' m_med$fit$r1_adj
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#' }
#'
#' @seealso [scf_ols()], [scf_glm()], [scf_MIcombine()], [quantreg::rq()]
#'
#' @references
#' Koenker R, Bassett G. Regression quantiles. \emph{Econometrica}.
#'   1978;46(1):33--50. \doi{10.2307/1913643}
#'
#' Koenker R, Machado JAF. Goodness of fit and related inference processes
#'   for quantile regression. \emph{Journal of the American Statistical
#'   Association}. 1999;94(448):1296--1310. \doi{10.2307/2669943}
#'
#' Rust KF, Rao JNK. Variance estimation for complex surveys using replication
#'   techniques. \emph{Statistical Methods in Medical Research}.
#'   1996;5(3):283--310. \doi{10.1177/096228029600500305}
#'
#' @importFrom stats weights pt coef as.formula
#' @importFrom survey withReplicates
#' @export
scf_quantreg <- function(object, formula, tau = 0.5,
                         se = c("nid", "iid", "ker", "boot", "replicate"),
                         ...) {
  
  if (!inherits(object, "scf_mi_survey"))
    stop("Input must be of class 'scf_mi_survey'.")
  if (!inherits(formula, "formula"))
    stop("'formula' must be a formula object.")
  if (!requireNamespace("quantreg", quietly = TRUE))
    stop("Package 'quantreg' is required.")
  if (!is.numeric(tau) || length(tau) != 1 || tau <= 0 || tau >= 1)
    stop("'tau' must be a single numeric value strictly between 0 and 1.")
  
  se <- match.arg(se)
  
  if (isTRUE(attr(object, "mock")))
    warning("Mock data detected. Do not interpret results as valid SCF estimates.",
            call. = FALSE)
  
  # Construct intercept-only (null) formula from the LHS of the full formula.
  # This is used to compute the Koenker-Machado R1(tau) goodness-of-fit statistic.
  lhs_chr   <- deparse(formula[[2]])
  null_formula <- stats::as.formula(paste(lhs_chr, "~ 1"))
  
  coefs_list <- list()
  vars_list  <- list()
  models     <- list()
  fit_errors <- character(0)
  
  # Per-implicate storage for goodness-of-fit components
  rho_vec      <- numeric(0)   # full model objective values
  rho_null_vec <- numeric(0)   # null model objective values
  nobs_vec     <- integer(0)   # sample sizes
  
  for (i in seq_along(object$mi_design)) {
    imp      <- object$mi_design[[i]]
    df       <- imp$variables
    df$.wts  <- as.numeric(stats::weights(imp, type = "sampling"))
    
    fit <- tryCatch(
      quantreg::rq(formula, tau = tau, data = df, weights = .wts,
                   method = "fn", ...),
      error = function(e) e
    )
    
    if (inherits(fit, "error")) {
      fit_errors <- c(fit_errors,
                      paste0("implicate ", i, ": rq failed: ",
                             conditionMessage(fit)))
      next
    }
    
    nm <- names(stats::coef(fit))
    
    if (se == "replicate") {
      p    <- length(nm)
      df_i <- df
      theta_fn <- function(w, ...) {
        df_i$.wts <- w
        r <- tryCatch(
          suppressWarnings(
            quantreg::rq(formula, tau = tau, data = df_i, weights = .wts)
          ),
          error = function(e) NULL
        )
        if (is.null(r)) return(rep(NA_real_, p))
        out <- rep(NA_real_, p)
        names(out) <- nm
        matched <- intersect(names(stats::coef(r)), nm)
        out[matched] <- stats::coef(r)[matched]
        out
      }
      cov_i <- stats::vcov(survey::withReplicates(imp, theta = theta_fn))
    } else {
      s     <- suppressWarnings(
        quantreg::summary.rq(fit, se = se, covariance = TRUE)
      )
      cov_i <- s$cov
    }
    
    dimnames(cov_i) <- list(nm, nm)
    coefs_list[[length(coefs_list) + 1]] <- stats::coef(fit)
    vars_list[[length(vars_list) + 1]]   <- cov_i
    models[[length(models) + 1]]         <- fit
    
    # --- Goodness-of-fit: full model objective value ---
    # sum(rho_tau(residuals)) where rho_tau is the check function.
    # quantreg stores the minimized objective in fit$rho (a scalar for single tau).
    rho_i <- if (!is.null(fit$rho)) {
      as.numeric(fit$rho)
    } else {
      # Fallback: compute directly from residuals and weights.
      resid_i <- as.numeric(stats::residuals(fit))
      wts_i   <- df$.wts
      sum(wts_i * ifelse(resid_i >= 0, tau * resid_i, (tau - 1) * resid_i))
    }
    
    # --- Goodness-of-fit: null model objective value ---
    fit_null <- tryCatch(
      quantreg::rq(null_formula, tau = tau, data = df, weights = .wts,
                   method = "fn"),
      error = function(e) NULL
    )
    
    rho_null_i <- if (!is.null(fit_null)) {
      if (!is.null(fit_null$rho)) {
        as.numeric(fit_null$rho)
      } else {
        resid_null <- as.numeric(stats::residuals(fit_null))
        wts_i      <- df$.wts
        sum(wts_i * ifelse(resid_null >= 0,
                           tau * resid_null, (tau - 1) * resid_null))
      }
    } else {
      NA_real_
    }
    
    rho_vec      <- c(rho_vec,      rho_i)
    rho_null_vec <- c(rho_null_vec, rho_null_i)
    nobs_vec     <- c(nobs_vec,     nrow(df))
  }
  
  if (length(models) < 2) {
    msg <- c("Too few successful implicate-level models to pool (need >= 2).",
             if (length(fit_errors)) c("Implicate errors:", fit_errors))
    stop(paste(msg, collapse = "\n"))
  }
  
  if (length(fit_errors) > 0)
    warning(paste(c("Some implicates failed:", fit_errors), collapse = "\n"),
            call. = FALSE)
  
  common_terms <- Reduce(intersect, lapply(coefs_list, names))
  if (length(common_terms) == 0)
    stop("No common coefficient terms found across implicates.")
  
  coefs_list <- lapply(coefs_list, function(x) x[common_terms])
  vars_list  <- lapply(vars_list, function(v) {
    v[common_terms, common_terms, drop = FALSE]
  })
  
  pooled <- scf_MIcombine(coefs_list, vars_list)
  
  est   <- pooled$coefficients
  se_v  <- sqrt(diag(pooled$variance))
  tval  <- est / se_v
  pval  <- 2 * stats::pt(-abs(tval), df = pooled$df)
  
  coef_table <- data.frame(
    term      = names(est),
    estimate  = unname(est),
    std.error = unname(se_v),
    t.value   = unname(tval),
    p.value   = unname(pval),
    stars     = as.character(cut(
      pval,
      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.10, Inf),
      labels = c("***", "**", "*", "^", ""),
      right  = FALSE
    )),
    stringsAsFactors = FALSE
  )
  
  # --- Pool goodness-of-fit statistics across successful implicates ---
  # rho and rho_null are averaged across implicates; R1 is computed from
  # the averages. This mirrors the pooling logic for coefficients (Rubin's
  # Rules average within-implicate quantities before combining).
  # NA values arise only if the null model failed for a given implicate;
  # those implicates are excluded from the fit statistic means.
  rho_mean      <- mean(rho_vec,      na.rm = TRUE)
  rho_null_mean <- mean(rho_null_vec, na.rm = TRUE)
  
  r1 <- if (!is.na(rho_null_mean) && rho_null_mean > 0) {
    1 - rho_mean / rho_null_mean
  } else {
    NA_real_
  }
  
  # Degrees-of-freedom-adjusted R1, analogous to adjusted R-squared.
  # n_mean = mean observations across implicates; p = number of estimated
  # parameters (length of common_terms, which includes the intercept).
  n_mean <- mean(nobs_vec)
  p      <- length(common_terms)
  
  r1_adj <- if (!is.na(r1) && (n_mean - p) > 0) {
    1 - (1 - r1) * n_mean / (n_mean - p)
  } else {
    NA_real_
  }
  
  fit_stats <- list(
    rho       = rho_mean,
    rho_null  = rho_null_mean,
    r1        = r1,
    r1_adj    = r1_adj,
    nobs      = nobs_vec,
    nobs_mean = n_mean
  )
  
  out <- list(
    results    = coef_table,
    tau        = tau,
    se_method  = se,
    fit        = fit_stats,
    vcov       = pooled$variance,
    models     = models,
    fit_errors = fit_errors,
    call       = match.call(),
    formula    = formula
  )
  class(out) <- c("scf_quantreg", "scf_model_result")
  return(out)
}


#' @export
#' @method print scf_quantreg
print.scf_quantreg <- function(x, digits = 4, ...) {
  cat(sprintf("Quantile Regression Results (tau = %.2f, Multiply-Imputed SCF)\n",
              x$tau))
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
  
  # Print goodness-of-fit if available
  fit <- x$fit
  if (!is.null(fit) && length(fit) > 0 && !is.null(fit$r1)) {
    cat(sprintf("R1(tau):   %-8s  R1 adj:  %s\n",
                if (is.na(fit$r1))     "NA" else formatC(fit$r1,     format = "f", digits = 4),
                if (is.na(fit$r1_adj)) "NA" else formatC(fit$r1_adj, format = "f", digits = 4)))
  }
  
  if (length(x$fit_errors) > 0)
    cat(sprintf("Warning: %d implicate(s) failed. See object$fit_errors.\n",
                length(x$fit_errors)))
  
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
  
  # Goodness-of-fit block
  fit <- object$fit
  if (!is.null(fit) && length(fit) > 0) {
    cat("\nGoodness of Fit (Koenker-Machado, 1999):\n")
    cat(sprintf("  Rho (full model):  %.4f\n", fit$rho))
    cat(sprintf("  Rho (null model):  %.4f\n", fit$rho_null))
    r1_str     <- if (is.na(fit$r1))     "NA" else sprintf("%.4f", fit$r1)
    r1_adj_str <- if (is.na(fit$r1_adj)) "NA" else sprintf("%.4f", fit$r1_adj)
    cat(sprintf("  R1(tau):           %s\n", r1_str))
    cat(sprintf("  R1(tau) adjusted:  %s\n", r1_adj_str))
    cat(sprintf("  Mean N (implicates): %.0f\n", fit$nobs_mean))
    cat("  Note: R1 is a local fit measure at tau; it is not a global\n")
    cat("  summary of fit across the conditional distribution.\n")
    cat("  R1 adjusted uses a df-penalty not derived from asymptotic\n")
    cat("  theory; interpret it descriptively.\n")
  }
  
  if (length(object$fit_errors) > 0) {
    cat("\nImplicate errors:\n")
    cat(paste(" ", object$fit_errors, collapse = "\n"), "\n")
  }
  
  cat("\nCall:\n")
  print(object$call)
  invisible(object)
}
