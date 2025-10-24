#' Estimate Generalized Linear Model from SCF Microdata
#'
#' @description
#' Estimates generalized linear models (GLMs) with SCF public-use microdata.
#' Use this function when modeling outcomes that follow non-Gaussian
#' distributions (e.g., binary or count data). Rubin's Rules are used to combine
#' implicate-level coefficient and variance estimates. 
#'
#' GLMs are performed across SCF implicates using `svyglm()` and returns
#' pooled coefficients, standard errors, z-values, p-values, and fit diagnostics
#' including AIC and pseudo-R-Squared when applicable.
#'
#' @section Implementation:
#' This function fits a GLM to each implicate in a `scf_mi_survey` object
#' using `survey::svyglm()`. The user specifies a model formula and a valid GLM
#' family (e.g., `binomial()`, `poisson()`, `gaussian()`). Coefficients and
#' variance-covariance matrices are extracted from each implicate and pooled
#' using Rubin's Rules.
#'
#' @section Details:
#' Generalized linear models (GLMs) extend linear regression to accommodate
#' non-Gaussian outcome distributions. The choice of `family` determines the
#' link function and error distribution. For example:
#' - `binomial()` fits logistic regression for binary outcomes
#' - `poisson()` models count data
#' - `gaussian()` recovers standard OLS behavior
#'
#' Model estimation is performed independently on each implicate using
#' `svyglm()` with replicate weights. Rubin's Rules are used to pool coefficient
#' estimates and variance matrices. For the pooling procedure, see
#' [scf_MIcombine()].
#'
#' @param object A `scf_mi_survey` object, typically created using [scf_load()] and [scf_design()].
#' @param formula A valid model formula, e.g., `rich ~ age + factor(edcl)`.
#' @param family A GLM family object such as [binomial()], [poisson()], or [gaussian()]. Defaults to `binomial()`.
#'
#' @return An object of class `"scf_glm"` and `"scf_model_result"` with:
#' \describe{
#'   \item{results}{A data frame of pooled coefficients, standard errors, z-values, p-values, and significance stars.}
#'   \item{fit}{A list of fit diagnostics including mean and SD of AIC; for binomial models, pseudo-R2 and its SD.}
#'   \item{models}{A list of implicate-level `svyglm` model objects.}
#'   \item{call}{The matched function call.}
#' }
#'
#' @examples
#' \donttest{
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td  <- tempdir()
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Run logistic regression
#' model <- scf_glm(scf2022, own ~ age + factor(edcl), family = binomial())
#' summary(model)
#' 
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink("scf2022.rds", force = TRUE)
#' }
#'
#' @section Internal Suppression:
#'
#' For CRAN compliance and to prevent diagnostic overload during package checks,
#' this function internally wraps each implicate-level model call in `suppressWarnings()`.
#' This suppresses the known benign warning:
#'
#'   `"non-integer #successes in a binomial glm!"`
#'
#' which arises from using replicate weights with `family = binomial()`. This suppression
#' does not affect model validity or inference. Users wishing to inspect warnings can
#' run `survey::svyglm()` directly on individual implicates via `model$models[[i]]`.
#'
#' For further background, see:
#' https://stackoverflow.com/questions/12953045/warning-non-integer-successes-in-a-binomial-glm-survey-packages
#'
#' @seealso [scf_ols()], [scf_logit()], [scf_regtable()]
#'
#' @export
scf_glm <- function(object, formula, family = binomial()) {
  if (!inherits(object, "scf_mi_survey"))
    stop("Input must be of class 'scf_mi_survey'")
  if (!inherits(formula, "formula"))
    stop("Model must be specified as a formula")
  if (!inherits(family, "family"))
    stop("Family must be a valid GLM family object (e.g., binomial())")

  if (isTRUE(attr(object, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }

  designs <- object$mi_design

  models <- lapply(designs, function(imp) {
    tryCatch(
      suppressWarnings(survey::svyglm(formula, design = imp, family = family)),
      error = function(e) NULL
    )
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
  zval <- est / se
  pval <- 2 * pnorm(-abs(zval))

  coef_table <- data.frame(
    term = names(est),
    estimate = est,
    std.error = se,
    z.value = zval,
    p.value = pval,
    stars = cut(pval,
                breaks = c(-Inf, 0.001, 0.01, 0.05, 0.10, Inf),
                labels = c("***", "**", "*", ".", ""),
                right = FALSE),
    stringsAsFactors = FALSE
  )

  aics <- sapply(models, function(m) {
    suppressWarnings(
      tryCatch(AIC(m), error = function(e) NA_real_)
    )
  })

  pseudo_r2 <- if (identical(family$family, "binomial")) {
    sapply(models, function(m) 1 - m$deviance / m$null.deviance)
  } else {
    rep(NA_real_, length(models))
  }

  fit_stats <- list(
    pseudo_r2 = if (all(!is.na(pseudo_r2))) mean(pseudo_r2) else NA_real_,
    pseudo_r2.sd = if (all(!is.na(pseudo_r2))) sd(pseudo_r2) else NA_real_,
    AIC = if (all(!is.na(aics))) mean(aics) else NA_real_,
    AIC.sd = if (all(!is.na(aics))) sd(aics) else NA_real_
  )

  out <- list(
    results = coef_table,
    fit = fit_stats,
    models = models,
    call = match.call()
  )
  class(out) <- c("scf_glm", "scf_model_result")
  return(out)
}
#' @export
print.scf_glm <- function(x, digits = 4, ...) {
  cat("Generalized Linear Model (Multiply-Imputed SCF)\n")
  cat("--------------------------------------------------\n")

  df <- x$results
  df$estimate   <- round(df$estimate, digits)
  df$std.error  <- round(df$std.error, digits)
  df$z.value    <- round(df$z.value, digits)
  df$p.value    <- format.pval(df$p.value, digits = digits)

  print(df[, c("term", "estimate", "std.error", "z.value", "p.value", "stars")], row.names = FALSE)

  cat("\nModel Fit Diagnostics:\n")
  if (!is.null(x$fit$pseudo_r2) && !is.na(x$fit$pseudo_r2)) {
    cat("  Pseudo R-squared: ", formatC(x$fit$pseudo_r2, digits = 3, format = "f"),
        " (SD: ", formatC(x$fit$pseudo_r2.sd, digits = 3, format = "f"), ")\n", sep = "")
  }
  if (!is.null(x$fit$AIC)) {
    cat("  Mean AIC:         ", formatC(x$fit$AIC, digits = 0, format = "f"),
        " (SD: ", formatC(x$fit$AIC.sd, digits = 0, format = "f"), ")\n", sep = "")
  }

  cat("\nNote: Model fit pooled across implicates via Rubin's Rules.\n")
  cat("      Inspect individual models via `object$models[[i]]`.\n")
  invisible(x)
}

#' @export
summary.scf_glm <- function(object, digits = 4, ...) {
  cat("SCF Generalized Linear Model Summary\n")
  cat("------------------------------------\n")

  df <- object$results
  df$estimate   <- round(df$estimate, digits)
  df$std.error  <- round(df$std.error, digits)
  df$z.value    <- round(df$z.value, digits)
  df$p.value    <- format.pval(df$p.value, digits = digits)

  cat("Pooled Coefficient Estimates:\n")
  print(df[, c("term", "estimate", "std.error", "z.value", "p.value", "stars")], row.names = FALSE)

  cat("\nModel Diagnostics:\n")
  if (!is.null(object$fit$pseudo_r2) && !is.na(object$fit$pseudo_r2)) {
    cat("  Pseudo R-squared: ", formatC(object$fit$pseudo_r2, digits = 3, format = "f"),
        " (SD: ", formatC(object$fit$pseudo_r2.sd, digits = 3, format = "f"), ")\n", sep = "")
  }
  if (!is.null(object$fit$AIC)) {
    cat("  Mean AIC:         ", formatC(object$fit$AIC, digits = 0, format = "f"),
        " (SD: ", formatC(object$fit$AIC.sd, digits = 0, format = "f"), ")\n", sep = "")
  }

  cat("\nCall:\n")
  print(object$call)

  invisible(object)
}
