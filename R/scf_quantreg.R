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
#'     This method is theoretically preferred for quantile regression: Shao and
#'     Wu (1992) establish asymptotic consistency of BRR variance estimators for
#'     sample quantiles, whereas analytical (sandwich) estimators are not
#'     guaranteed consistent for nonsmooth statistics (Rust and Rao, 1996).
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
#' @section Implementation:
#' For each implicate:
#' \enumerate{
#'   \item Extracts the survey data and final sampling weights from the
#'     `svyrep.design` object.
#'   \item Fits [quantreg::rq()] at quantile `tau` with the final weights.
#'   \item Estimates the within-implicate variance-covariance matrix using
#'     the method specified by `se`.
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
#'   \item{`fit`}{An empty list (AIC is not defined for quantile regression).}
#'   \item{`models`}{A list of implicate-level `rq` model objects for
#'     direct inspection.}
#'   \item{`fit_errors`}{Character vector of any implicate-level errors.}
#'   \item{`call`}{The matched call.}
#'   \item{`formula`}{The model formula.}
#' }
#'
#' @examples
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
#' # Example for real analysis: 75th-percentile regression
#' m_75 <- scf_quantreg(scf2022, networth ~ age + factor(edcl), tau = 0.75)
#' summary(m_75)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @seealso [scf_ols()], [scf_glm()], [scf_MIcombine()], [quantreg::rq()]
#'
#' @references
#' Koenker R, Bassett G. Regression quantiles. \emph{Econometrica}.
#'   1978;46(1):33--50. \doi{10.2307/1913643}
#'
#' Koenker R. \emph{Quantile Regression}. Cambridge University Press; 2005.
#'   \doi{10.1017/CBO9780511754098}
#'
#' Rust KF, Rao JNK. Variance estimation for complex surveys using replication
#'   techniques. \emph{Statistical Methods in Medical Research}.
#'   1996;5(3):283--310. \doi{10.1177/096228029600500305}
#'
#' Shao J, Wu CFJ. Asymptotic properties of the balanced repeated replication
#'   method for sample quantiles. \emph{Annals of Statistics}.
#'   1992;20(3):1371--1393. \doi{10.1214/aos/1176348781}
#'
#' @importFrom stats weights pt coef
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
  
  coefs_list <- list()
  vars_list  <- list()
  models     <- list()
  fit_errors <- character(0)
  
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
  
  out <- list(
    results    = coef_table,
    tau        = tau,
    se_method  = se,
    fit        = list(),
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
  
  if (length(object$fit_errors) > 0) {
    cat("\nImplicate errors:\n")
    cat(paste(" ", object$fit_errors, collapse = "\n"), "\n")
  }
  
  cat("\nCall:\n")
  print(object$call)
  invisible(object)
}