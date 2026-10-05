#' Estimate a Quantile Regression Model on SCF Microdata
#'
#' Fits a weighted quantile regression to each implicate with
#' [quantreg::rq()] and pools the results with Rubin's rules.
#'
#' @details
#' Use it for skewed outcomes such as wealth and income, where the mean is
#' pulled by a few very wealthy households. Each coefficient is the change in
#' the `tau` quantile of the outcome per one-unit increase in the predictor;
#' with `tau = 0.5`, the change in the median. Comparing several values of
#' `tau` shows whether a predictor matters more at the top or bottom of the
#' distribution. Estimate each quantile in a separate call. P-values use a t
#' distribution with Rubin's degrees of freedom. The function stops if the
#' predictors are collinear.
#'
#' `se` sets the within-implicate covariance:
#' \describe{
#'   \item{`"replicate"` (default)}{Refits the model with each of the 999
#'     replicate weights (see [scf_variance]). The only method that uses the
#'     SCF's design, and consistent for quantiles (Rust and Rao 1996). About
#'     5,000 fits; several minutes on the full data.}
#'   \item{`"nid"`, `"iid"`, `"ker"`, `"boot"`}{From
#'     [quantreg::summary.rq()]. These treat the weighted sample as a simple
#'     random sample. `"nid"` allows the error density to vary; `"iid"`
#'     assumes it is constant (Koenker and Bassett 1978); `"ker"` uses a kernel
#'     estimate; `"boot"` is a pairs bootstrap. Analytic methods are unreliable
#'     at quantiles with mass points, such as low quantiles of net worth.}
#' }
#'
#' Fit statistics (Koenker and Machado 1999) compare the model with an
#' intercept-only model at the same `tau`:
#' \describe{
#'   \item{`rho`, `rho_null`}{Weighted check-loss of the model and the null
#'     model, averaged across implicates. Weights are scaled to mean 1.}
#'   \item{`r1`}{\eqn{1 - \rho / \rho_{null}}. A local measure of fit at
#'     `tau`, from 0 to 1.}
#'   \item{`r1_adj`}{\eqn{1 - (1 - R^1) n / (n - p)}. A descriptive
#'     adjustment, not derived from Koenker and Machado.}
#'   \item{`nobs`, `nobs_mean`}{Observations per implicate and their mean.}
#' }
#'
#' @param object A `scf_mi_survey` object created with [scf_load()].
#' @param formula A model formula specifying the outcome and predictors,
#'   e.g., `networth ~ age + factor(edcl)`. The outcome should be numeric.
#' @param tau Quantile to estimate, between 0 and 1. Default 0.5 (median).
#' @param se Within-implicate covariance method: `"replicate"` (default),
#'   `"nid"`, `"iid"`, `"ker"`, or `"boot"`. See Details.
#' @param ... Additional arguments passed to [quantreg::rq()].
#'
#' @return An object of class `"scf_quantreg"` and `"scf_model_result"` with:
#' \describe{
#'   \item{`results`}{A data frame of pooled coefficients, standard errors,
#'     t-values, p-values, and significance stars.}
#'   \item{`tau`}{The quantile estimated.}
#'   \item{`se_method`}{The SE method used.}
#'   \item{`fit`}{Fit statistics: `rho`, `rho_null`, `r1`, `r1_adj`,
#'     `nobs`, `nobs_mean`. See Details.}
#'   \item{`models`}{A list of implicate-level `rq` model objects for
#'     direct inspection.}
#'   \item{`imp_vcov`}{A list of implicate-level variance-covariance matrices.}
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
                         se = c("replicate", "nid", "iid", "ker", "boot"),
                         ...) {

  if (!inherits(object, "scf_mi_survey"))
    stop("Input must be of class 'scf_mi_survey'.")
  if (!inherits(formula, "formula"))
    stop("'formula' must be a formula object.")
  if (!is.numeric(tau) || length(tau) != 1 || tau <= 0 || tau >= 1)
    stop("'tau' must be a single numeric value strictly between 0 and 1.")

  se <- match.arg(se)
  .scf_check_na(object$mi_design, formula)

  lhs_chr <- deparse(formula[[2]])
  null_formula <- stats::as.formula(paste(lhs_chr, "~ 1"))

  coefs_list <- list()
  vars_list <- list()
  models <- list()

  rho_vec <- numeric(0)
  rho_null_vec <- numeric(0)
  nobs_vec <- integer(0)

  rq_dots <- list(...)
  fit_rq <- function(data, f = formula, ...) {
    quantreg::rq(f, tau = tau, data = data, weights = .wts, method = "fn", ...)
  }

  check_loss <- function(m) {
    r <- as.numeric(m$residuals)
    w <- m$weights / mean(m$weights)
    sum(w * r * (tau - (r < 0)))
  }

  for (i in seq_along(object$mi_design)) {
    imp <- object$mi_design[[i]]
    df <- imp$variables
    df$.wts <- as.numeric(stats::weights(imp, type = "sampling"))
    df$.wts <- df$.wts / mean(df$.wts)

    X <- stats::model.matrix(formula, df)
    if (qr(X)$rank < ncol(X)) {
      stop(sprintf(paste0(
        "The model failed in implicate %d: the predictors are linearly dependent. ",
        "Check for terms that are perfectly collinear."), i), call. = FALSE)
    }

    fit <- tryCatch(
      do.call(fit_rq, c(list(df), rq_dots)),
      error = function(e) .scf_fit_error(i, e)
    )

    nm <- names(stats::coef(fit))

    if (se == "replicate") {
      df_i <- df
      theta_fn <- function(w, ...) {
        df_i$.wts <- w
        r <- tryCatch(
          suppressWarnings(do.call(fit_rq, c(list(df_i), rq_dots))),
          error = function(e) .scf_fit_error(i, e)
        )
        stats::coef(r)
      }
      cov_i <- stats::vcov(survey::withReplicates(imp, theta = theta_fn))
    } else {
      s <- tryCatch(
        suppressWarnings(quantreg::summary.rq(fit, se = se, covariance = TRUE)),
        error = function(e) .scf_fit_error(i, e)
      )
      cov_i <- s$cov
    }

    dimnames(cov_i) <- list(nm, nm)
    coefs_list[[i]] <- stats::coef(fit)
    vars_list[[i]] <- cov_i
    models[[i]] <- fit

    rho_i <- check_loss(fit)

    df_null <- if (is.null(fit$na.action)) df else df[-fit$na.action, ]
    fit_null <- tryCatch(
      fit_rq(df_null, null_formula),
      error = function(e) .scf_fit_error(i, e)
    )
    rho_null_i <- check_loss(fit_null)

    rho_vec <- c(rho_vec, rho_i)
    rho_null_vec <- c(rho_null_vec, rho_null_i)
    nobs_vec <- c(nobs_vec, length(fit$residuals))
  }

  aligned <- .scf_align_terms(coefs_list, vars_list)
  coefs_list <- aligned$coefs
  vars_list <- aligned$vars

  pooled <- scf_MIcombine(coefs_list, vars_list)

  est <- pooled$coefficients
  se_v <- sqrt(diag(pooled$variance))
  tval <- est / se_v
  pval <- 2 * stats::pt(-abs(tval), df = pooled$df)

  coef_table <- data.frame(
    term = names(est),
    estimate = unname(est),
    std.error = unname(se_v),
    t.value = unname(tval),
    p.value = unname(pval),
    stars = as.character(.scf_stars(pval)),
    stringsAsFactors = FALSE
  )

  rho_mean <- mean(rho_vec, na.rm = TRUE)
  rho_null_mean <- mean(rho_null_vec, na.rm = TRUE)

  r1 <- if (!is.na(rho_null_mean) && rho_null_mean > 0) {
    1 - rho_mean / rho_null_mean
  } else {
    NA_real_
  }

  n_mean <- mean(nobs_vec)
  p <- length(coefs_list[[1]])

  r1_adj <- if (!is.na(r1) && (n_mean - p) > 0) {
    1 - (1 - r1) * n_mean / (n_mean - p)
  } else {
    NA_real_
  }

  fit_stats <- list(
    rho = rho_mean,
    rho_null = rho_null_mean,
    r1 = r1,
    r1_adj = r1_adj,
    nobs = nobs_vec,
    nobs_mean = n_mean
  )

  out <- list(
    results = coef_table,
    tau = tau,
    se_method = se,
    fit = fit_stats,
    vcov = pooled$variance,
    df = unname(pooled$df),
    models = models,
    imp_vcov = vars_list,
    call = match.call(),
    formula = formula
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
  df$estimate <- round(df$estimate, digits)
  df$std.error <- round(df$std.error, digits)
  df$t.value <- round(df$t.value, digits)
  df$p.value <- format.pval(df$p.value, digits = digits)

  print(df[, c("term", "estimate", "std.error", "t.value", "p.value", "stars")],
        row.names = FALSE)

  cat(sprintf("\nSE method: %s | Implicates pooled: %d\n",
              x$se_method, length(x$models)))

  fit <- x$fit
  if (!is.null(fit) && length(fit) > 0 && !is.null(fit$r1)) {
    cat(sprintf("R1(tau):   %-8s  R1 adj:  %s\n",
                if (is.na(fit$r1)) "NA" else formatC(fit$r1, format = "f", digits = 4),
                if (is.na(fit$r1_adj)) "NA" else formatC(fit$r1_adj, format = "f", digits = 4)))
  }

  cat("Implicate-level rq objects stored in `object$models`.\n")
  invisible(x)
}

#' @export
#' @method summary scf_quantreg
summary.scf_quantreg <- function(object, digits = 4, ...) {
  cat(sprintf("SCF Quantile Regression Summary (tau = %.2f)\n", object$tau))
  cat("------------------------------------------------------------------\n")

  df <- object$results
  df$estimate <- round(df$estimate, digits)
  df$std.error <- round(df$std.error, digits)
  df$t.value <- round(df$t.value, digits)
  df$p.value <- format.pval(df$p.value, digits = digits)

  cat("Pooled Coefficient Estimates:\n")
  print(df[, c("term", "estimate", "std.error", "t.value", "p.value", "stars")],
        row.names = FALSE)

  cat(sprintf(
    "\nQuantile:        %.2f\nSE method:       %s\nImplicates used: %d\n",
    object$tau, object$se_method, length(object$models)
  ))

  fit <- object$fit
  if (!is.null(fit) && length(fit) > 0) {
    cat("\nGoodness of Fit (Koenker-Machado, 1999):\n")
    cat(sprintf("  Rho (full model):  %.4f\n", fit$rho))
    cat(sprintf("  Rho (null model):  %.4f\n", fit$rho_null))
    r1_str <- if (is.na(fit$r1)) "NA" else sprintf("%.4f", fit$r1)
    r1_adj_str <- if (is.na(fit$r1_adj)) "NA" else sprintf("%.4f", fit$r1_adj)
    cat(sprintf("  R1(tau):           %s\n", r1_str))
    cat(sprintf("  R1(tau) adjusted:  %s\n", r1_adj_str))
    cat(sprintf("  Mean N (implicates): %.0f\n", fit$nobs_mean))
    cat("  Note: R1 is a local fit measure at tau; it is not a global\n")
    cat("  summary of fit across the conditional distribution.\n")
    cat("  R1 adjusted uses a df-penalty not derived from asymptotic\n")
    cat("  theory; interpret it descriptively.\n")
  }

  cat("\nCall:\n")
  print(object$call)
  invisible(object)
}
