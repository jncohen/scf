#' Estimate Generalized Linear Model from SCF Microdata
#'
#' Fits a generalized linear model to each implicate with `survey::svyglm()`
#' and pools the results with Rubin's rules.
#'
#' @details
#' Use it for outcomes that are not continuous, such as binary outcomes or
#' counts. `family` sets the link and error distribution: `binomial()` for
#' binary outcomes, `poisson()` for counts, `gaussian()` for linear
#' regression. For odds ratios, use [scf_logit()].
#'
#' Coefficients are on the link scale (log-odds for `binomial()`, log counts
#' for `poisson()`). A positive coefficient means the outcome rises with the
#' predictor, holding the others constant. The p-value tests whether a
#' coefficient is zero, using a t distribution with Rubin's degrees of
#' freedom. AIC is averaged across implicates; compare it only across models
#' of the same outcome, where lower is better. Pooling is described in
#' [scf_MIcombine()].
#'
#' Each distinct warning from `svyglm()` is reported once, with the implicates
#' it came from. "non-integer #successes in a binomial glm!", which survey
#' weights always produce, is dropped. The function stops if the model fails
#' in any implicate.
#'
#' @param object A `scf_mi_survey` object, created with [scf_load()].
#' @param formula A valid model formula, e.g., `rich ~ age + factor(edcl)`.
#' @param family A GLM family object such as [binomial()], [poisson()], or [gaussian()]. Defaults to `binomial()`.
#'
#' @return An object of class `"scf_glm"` and `"scf_model_result"` with:
#' \describe{
#'   \item{results}{A data frame of pooled coefficients, standard errors, t-values, p-values, and significance stars. P-values use a t distribution with Rubin's degrees of freedom (see [scf_MIcombine]).}
#'   \item{fit}{Fit statistics: mean and SD of AIC (`AIC`, `AIC.sd`) across implicates, pseudo-R-squared (`pseudo_r2`, `pseudo_r2.sd`; binomial models only), observations per implicate (`nobs`), and their mean (`nobs_mean`).}
#'   \item{models}{A list of implicate-level `svyglm` model objects.}
#'   \item{call}{The matched function call.}
#' }
#'
#' @examples
#' \donttest{
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("glm_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Run logistic regression
#' model <- scf_glm(scf2022, own ~ hhsex, family = binomial())
#' summary(model)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#' }
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

  designs <- object$mi_design

  .scf_check_na(designs, formula)

  warn_msg <- character(0)
  warn_imp <- integer(0)
  models <- lapply(seq_along(designs), function(i) {
    withCallingHandlers(
      tryCatch(
        survey::svyglm(formula, design = designs[[i]], family = family),
        error = function(e) .scf_fit_error(i, e)
      ),
      warning = function(w) {
        msg <- conditionMessage(w)
        if (!grepl("non-integer #successes", msg)) {
          warn_msg <<- c(warn_msg, msg)
          warn_imp <<- c(warn_imp, i)
        }
        invokeRestart("muffleWarning")
      }
    )
  })
  for (msg in unique(warn_msg)) {
    imps <- sort(unique(warn_imp[warn_msg == msg]))
    warning(sprintf("%s (implicate%s %s)", msg, if (length(imps) > 1) "s" else "",
                    paste(imps, collapse = ", ")), call. = FALSE)
  }

  coefs_list <- lapply(models, coef)
  vars_list <- lapply(models, vcov)
  aligned <- .scf_align_terms(coefs_list, vars_list)
  coefs_list <- aligned$coefs
  vars_list <- aligned$vars

  pooled <- scf_MIcombine(coefs_list, vars_list)

  est <- coef(pooled)
  se <- SE(pooled)
  tval <- est / se
  pval <- 2 * pt(-abs(tval), df = pooled$df)

  coef_table <- data.frame(
    term = names(est),
    estimate = est,
    std.error = se,
    t.value = tval,
    p.value = pval,
    stars = .scf_stars(pval),
    stringsAsFactors = FALSE
  )

  aics <- sapply(models, function(m) {
    tryCatch(suppressWarnings(AIC(m)[["AIC"]]), error = function(e) {
      warning("AIC could not be computed for an implicate: ",
              conditionMessage(e), call. = FALSE)
      NA_real_
    })
  })

  models <- lapply(models, function(m) { m$survey.design <- NULL; m$data <- NULL; m })

  pseudo_r2 <- if (identical(family$family, "binomial")) {
    sapply(models, function(m) 1 - m$deviance / m$null.deviance)
  } else {
    rep(NA_real_, length(models))
  }

  nobs_vec <- sapply(models, nobs)

  fit_stats <- list(
    pseudo_r2 = if (all(!is.na(pseudo_r2))) mean(pseudo_r2) else NA_real_,
    pseudo_r2.sd = if (all(!is.na(pseudo_r2))) sd(pseudo_r2) else NA_real_,
    AIC = if (all(!is.na(aics))) mean(aics) else NA_real_,
    AIC.sd = if (all(!is.na(aics))) sd(aics) else NA_real_,
    nobs = nobs_vec,
    nobs_mean = mean(nobs_vec)
  )

  out <- list(
    results = coef_table,
    fit = fit_stats,
    vcov = pooled$variance,
    df = unname(pooled$df),
    models = models,
    call = match.call(),
    formula = formula
  )
  class(out) <- c("scf_glm", "scf_model_result")
  return(out)
}
#' @export
print.scf_glm <- function(x, digits = 4, ...) {
  cat("Generalized Linear Model (Multiply-Imputed SCF)\n")
  cat("--------------------------------------------------\n")

  df <- x$results
  df$estimate <- round(df$estimate, digits)
  df$std.error <- round(df$std.error, digits)
  df$t.value <- round(df$t.value, digits)
  df$p.value <- format.pval(df$p.value, digits = digits)

  print(df[, c("term", "estimate", "std.error", "t.value", "p.value", "stars")], row.names = FALSE)

  cat("\nModel Fit Diagnostics:\n")
  if (!is.null(x$fit$pseudo_r2) && !is.na(x$fit$pseudo_r2)) {
    cat("  Pseudo R-squared: ", formatC(x$fit$pseudo_r2, digits = 3, format = "f"),
        " (SD: ", formatC(x$fit$pseudo_r2.sd, digits = 3, format = "f"), ")\n", sep = "")
  }
  if (!is.null(x$fit$AIC)) {
    cat("  Mean AIC:         ", formatC(x$fit$AIC, digits = 0, format = "f"),
        " (SD: ", formatC(x$fit$AIC.sd, digits = 0, format = "f"), ")\n", sep = "")
  }

  cat("\nNote: Coefficients pooled with Rubin's rules; fit statistics averaged across implicates.\n")
  cat("      Inspect individual models via `object$models[[i]]`.\n")
  invisible(x)
}

#' @export
summary.scf_glm <- function(object, digits = 4, ...) {
  cat("SCF Generalized Linear Model Summary\n")
  cat("------------------------------------\n")

  df <- object$results
  df$estimate <- round(df$estimate, digits)
  df$std.error <- round(df$std.error, digits)
  df$t.value <- round(df$t.value, digits)
  df$p.value <- format.pval(df$p.value, digits = digits)

  cat("Pooled Coefficient Estimates:\n")
  print(df[, c("term", "estimate", "std.error", "t.value", "p.value", "stars")], row.names = FALSE)

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
