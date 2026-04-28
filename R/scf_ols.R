#' Estimate an Ordinary Least Squares Regression on SCF Microdata
#'
#' Fits a replicate-weighted linear regression model to each implicate of
#' multiply-imputed SCF data and pools coefficients and standard errors using
#' Rubin’s Rules.
#'
#' @description
#' Computes an OLS regression on SCF data using `svyglm()` across the SCF's
#' five implicates.  Returns coefficient estimates, standard errors, test
#' statistics, and model diagnostics.
#'
#' @section Implementation:
#' Ordinary least squares (OLS) regression estimates the linear relationship
#' between a continuous outcome and one or more predictor variables. Each
#' coefficient represents the expected change in the outcome for a one-unit
#' increase in the corresponding predictor, holding all other predictors
#' constant. 
#'
#' Use this function to model associations between SCF variables while
#' accounting for complex survey design and multiple imputation.
#'
#' This function takes a `scf_mi_survey` object and a model formula. Internally,
#' it fits a weighted linear regression to each implicate using
#' `survey::svyglm()`, extracts coefficients and variance-covariance matrices,
#' and pools them via [scf_MIcombine()].
#'
#' @param object A `scf_mi_survey` object created with [scf_load()] and [scf_design()]. Must contain five implicates with replicate weights.
#' @param formula A model formula specifying a continuous outcome and predictor variables (e.g., `networth ~ income + age`).
#'
#' @return An object of class `"scf_ols"` and `"scf_model_result"` with:
#' \describe{
#'   \item{results}{A data frame of pooled coefficients, standard errors, t-values, p-values, and significance stars.}
#'   \item{fit}{A list of model diagnostics including mean AIC, standard deviation of AIC, mean R-squared, and its standard deviation.}
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
#' @importFrom stats coef vcov pt sd AIC deviance
#'
#' @export
scf_ols <- function(object, formula) {
  if (!inherits(object, "scf_mi_survey"))
    stop("Input must be of class 'scf_mi_survey'")
  if (!inherits(formula, "formula"))
    stop("Model must be specified as a formula")

  if (isTRUE(attr(object, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }


  models <- lapply(object$mi_design, function(imp) {
    tryCatch(survey::svyglm(formula, design = imp), error = function(e) NULL)
  })

  models <- Filter(Negate(is.null), models)
  if (length(models) < 2)
    stop("Too few successful implicate-level models to pool.")

  coefs_list <- lapply(models, coef)
  vars_list  <- lapply(models, vcov)
  common_terms <- Reduce(intersect, lapply(coefs_list, names))
  coefs_list <- lapply(coefs_list, function(x) x[common_terms])
  vars_list  <- lapply(vars_list, function(v) v[common_terms, common_terms, drop = FALSE])

  pooled <- scf_MIcombine(coefs_list, vars_list)

  est  <- coef(pooled)
  se   <- SE(pooled)
  tval <- est / se
  pval <- 2 * pt(-abs(tval), df = pooled$df)

  coefs <- data.frame(
    term = names(est),
    estimate = est,
    std.error = se,
    t.value = tval,
    p.value = pval,
    stars = cut(pval,
                breaks = c(-Inf, 0.001, 0.01, 0.05, 0.10, Inf),
                labels = c("***", "**", "*", "^", ""),
                right = FALSE),
    stringsAsFactors = FALSE
  )

  aics <- sapply(models, AIC)
  r2s  <- sapply(models, function(m) {
    if (is.null(m$null.deviance) || m$null.deviance == 0) return(NA_real_)
    1 - deviance(m) / m$null.deviance
  })


  diagnostics <- list(
    AIC = mean(aics),
    AIC.sd = sd(aics),
    r.squared = mean(r2s),
    r.squared.sd = sd(r2s)
  )

  out <- list(
    results = coefs,
    fit = diagnostics,
    vcov = pooled$variance,
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
  df$estimate   <- round(df$estimate,   digits)
  df$std.error  <- round(df$std.error,  digits)
  df$t.value    <- round(df$t.value,    digits)
  df$p.value    <- format.pval(df$p.value, digits = digits)

  cat("Pooled Coefficient Estimates:\n")
  print(df[, c("term", "estimate", "std.error", "t.value", "p.value", "stars")],
        row.names = FALSE)

  cat("\nModel Fit Statistics:\n")
  with(object$fit, {
    if (!is.null(r.squared) && !is.na(r.squared)) {
      cat("  Mean R-squared: ", round(r.squared, digits),
          " (SD: ", round(r.squared.sd, digits), ")\n", sep = "")
    }
    cat("  Mean AIC:       ", round(AIC, digits),
        " (SD: ", round(AIC.sd, digits), ")\n", sep = "")
  })

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
  with(x$fit, {
    if (!is.null(r.squared)) {
      cat("  Mean R-squared: ", round(r.squared, digits),
          " (SD: ", round(r.squared.sd, digits), ")\n", sep = "")
    }
    cat("  Mean AIC:       ", round(AIC, digits),
        " (SD: ", round(AIC.sd, digits), ")\n", sep = "")
  })

  cat("\nNote: Implicate-level model objects are stored in `object$imps`\n")
  cat("      Use `summary(object$imps[[1]])` to inspect them.\n")
  invisible(x)
}

