#' Estimate an Ordinary Least Squares Regression on SCF Microdata
#'
#' Fits a linear regression to each implicate with `survey::svyglm()` and
#' pools the results with Rubin's rules.
#'
#' @details
#' Use it for continuous outcomes. For binary outcomes use [scf_logit()]; for
#' skewed outcomes such as wealth, consider [scf_quantreg()] or a log
#' transform.
#'
#' Each coefficient is the expected change in the outcome for a one-unit
#' increase in the predictor, holding the others constant. The p-value tests
#' whether a coefficient is zero, using a t distribution with Rubin's degrees
#' of freedom. R-squared is the share of variance explained and AIC compares
#' models of the same outcome (lower is better); both are averaged across
#' implicates. Pooling is described in [scf_MIcombine()].
#'
#' @param object A `scf_mi_survey` object created with [scf_load()].
#' @param formula A model formula specifying a continuous outcome and predictor variables (e.g., `networth ~ income + age`).
#'
#' @return An object of class `"scf_ols"` and `"scf_model_result"` with:
#' \describe{
#'   \item{results}{A data frame of pooled coefficients, standard errors, t-values, p-values, and significance stars.}
#'   \item{fit}{Fit statistics: mean and SD of AIC (`AIC`, `AIC.sd`) and R-squared (`r.squared`, `r.squared.sd`) across implicates, observations per implicate (`nobs`), and their mean (`nobs_mean`).}
#'   \item{imps}{A list of implicate-level `svyglm` model objects.}
#'   \item{call}{The matched call used to produce the model.}
#' }
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("ols_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Run OLS model
#' model <- scf_ols(scf2022, networth ~ income + age)
#' summary(model)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @seealso [scf_glm()], [scf_logit()], [scf_MIcombine()]
#' @importFrom stats coef vcov pt sd AIC deviance nobs
#'
#' @export
scf_ols <- function(object, formula) {
  if (!inherits(object, "scf_mi_survey"))
    stop("Input must be of class 'scf_mi_survey'")
  if (!inherits(formula, "formula"))
    stop("Model must be specified as a formula")

  .scf_check_na(object$mi_design, formula)

  models <- lapply(seq_along(object$mi_design), function(i) {
    tryCatch(survey::svyglm(formula, design = object$mi_design[[i]]),
             error = function(e) .scf_fit_error(i, e))
  })

  coefs_list <- lapply(models, coef)
  vars_list <- lapply(models, vcov)
  aligned <- .scf_align_terms(coefs_list, vars_list)
  coefs_list <- aligned$coefs
  vars_list <- aligned$vars

  models <- lapply(models, .scf_lighten_fit, env = environment(formula))

  pooled <- scf_MIcombine(coefs_list, vars_list)

  est <- coef(pooled)
  se <- SE(pooled)
  tval <- est / se
  pval <- 2 * pt(-abs(tval), df = pooled$df)

  coefs <- data.frame(
    term = names(est),
    estimate = est,
    std.error = se,
    t.value = tval,
    p.value = pval,
    stars = .scf_stars(pval),
    stringsAsFactors = FALSE
  )

  aics <- sapply(models, function(m) AIC(m)[["AIC"]])
  r2s <- sapply(models, function(m) {
    if (is.null(m$null.deviance) || m$null.deviance == 0) return(NA_real_)
    1 - deviance(m) / m$null.deviance
  })

  nobs_vec <- sapply(models, nobs)

  diagnostics <- list(
    AIC = mean(aics),
    AIC.sd = sd(aics),
    r.squared = mean(r2s),
    r.squared.sd = sd(r2s),
    nobs = nobs_vec,
    nobs_mean = mean(nobs_vec)
  )

  out <- list(
    results = coefs,
    fit = diagnostics,
    vcov = pooled$variance,
    df = unname(pooled$df),
    imps = models,
    call = match.call(),
    formula = formula
  )
  class(out) <- c("scf_ols", "scf_model_result")
  return(out)
}
#' @export
#' @method summary scf_ols
summary.scf_ols <- function(object, digits = 4, ...) {
  cat("SCF OLS Regression Summary\n")
  cat("--------------------------------------------------\n")

  df <- object$results
  df$estimate <- round(df$estimate, digits)
  df$std.error <- round(df$std.error, digits)
  df$t.value <- round(df$t.value, digits)
  df$p.value <- format.pval(df$p.value, digits = digits)

  cat("Pooled Coefficient Estimates:\n")
  print(df[, c("term", "estimate", "std.error", "t.value", "p.value", "stars")],
        row.names = FALSE)

  cat("\nModel Fit Statistics:\n")
  fit <- object$fit
  if (!is.null(fit$r.squared) && !is.na(fit$r.squared)) {
    cat("  Mean R-squared: ", round(fit$r.squared, digits),
        " (SD: ", round(fit$r.squared.sd, digits), ")\n", sep = "")
  }
  cat("  Mean AIC:       ", round(fit$AIC, digits),
      " (SD: ", round(fit$AIC.sd, digits), ")\n", sep = "")

  cat("\nCall:\n")
  print(object$call)
  invisible(object)
}

#' @export
#' @method print scf_ols
print.scf_ols <- function(x, digits = 4, ...) {
  cat("OLS Regression Results (Multiply-Imputed SCF)\n")
  cat("--------------------------------------------------\n")
  print(x$results, digits = digits, row.names = FALSE)

  cat("\nModel Fit Statistics:\n")
  fit <- x$fit
  if (!is.null(fit$r.squared)) {
    cat("  Mean R-squared: ", round(fit$r.squared, digits),
        " (SD: ", round(fit$r.squared.sd, digits), ")\n", sep = "")
  }
  cat("  Mean AIC:       ", round(fit$AIC, digits),
      " (SD: ", round(fit$AIC.sd, digits), ")\n", sep = "")

  cat("\nNote: Implicate-level model objects are stored in `object$imps`\n")
  cat("      Use `summary(object$imps[[1]])` to inspect them.\n")
  invisible(x)
}

