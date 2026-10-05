#' Estimate Logistic Regression Model using SCF Microdata
#'
#' Fits a logistic regression to each implicate and pools the results with
#' Rubin's rules. Returns odds ratios by default.
#'
#' @details
#' Use it for binary outcomes, such as homeownership. An odds ratio above 1
#' means higher odds of the outcome per one-unit increase in the predictor;
#' below 1, lower odds. An odds ratio of 1.5 means 50 percent higher odds. The
#' p-value tests an odds ratio of 1 (no association).
#'
#' Calls [scf_glm()] with `family = binomial()`. With `odds = TRUE`,
#' estimates are exponentiated and standard errors converted to the
#' odds-ratio scale. `vcov()` stays on the log-odds scale; `confint()` computes
#' intervals on that scale and exponentiates them.
#'
#' @param object A `scf_mi_survey` object created with [scf_load()].
#' @param formula A model formula specifying a binary outcome and predictors, e.g., `rich ~ age + factor(edcl)`.
#' @param odds Logical. If `TRUE` (default), exponentiates coefficient estimates to produce odds ratios.
#'
#' @return An object of class `"scf_logit"` and `"scf_model_result"` with:
#' \describe{
#'   \item{results}{A data frame of pooled estimates (log-odds or odds ratios), standard errors, and test statistics.}
#'   \item{fit}{Fit statistics: mean and SD of AIC (`AIC`, `AIC.sd`) and pseudo-R-squared (`pseudo_r2`, `pseudo_r2.sd`) across implicates, observations per implicate (`nobs`), and their mean (`nobs_mean`).}
#'   \item{imps}{List of implicate-level `svyglm` model objects.}
#'   \item{call}{The matched function call.}
#' }
#'
#' @examples
#' \donttest{
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("logit_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Run logistic regression
#' model <- scf_logit(scf2022, own ~ hhsex)
#' summary(model)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#' }
#'
#' @seealso [scf_glm()], [scf_ols()], [scf_MIcombine()]
#'
#' @export
scf_logit <- function(object, formula, odds = TRUE) {
  model <- scf_glm(object, formula, family = binomial())

  if (odds) {
    logit_se <- model$results$std.error
    model$results$estimate <- exp(model$results$estimate)
    model$results$std.error <- model$results$estimate * logit_se
    attr(model$results, "scale") <- "odds"
  } else {
    attr(model$results, "scale") <- "logit"
  }

  out <- list(
    results = model$results,
    imps = model$models,
    fit = model$fit,
    vcov = model$vcov,
    df = model$df,
    call = match.call(),
    formula = formula
  )
  class(out) <- c("scf_logit", "scf_model_result")
  return(out)
}

#' @export
summary.scf_logit <- function(object, digits = 4, ...) {
  cat("SCF Logistic Regression Summary\n")
  cat("---------------------------------\n")
  scale <- attr(object$results, "scale")
  est_label <- if (!is.null(scale) && scale == "odds") "Odds Ratio" else "Log-Odds"

  cat("Outcome modeled using logit link.\n")
  cat("Estimates are on the", est_label, "scale.\n\n")

  df <- object$results
  df$estimate <- round(df$estimate, digits)
  df$std.error <- round(df$std.error, digits)
  df$t.value <- round(df$t.value, digits)
  df$p.value <- format.pval(df$p.value, digits = digits)

  cat("Coefficient Table:\n")
  print(df[, c("term", "estimate", "std.error", "t.value", "p.value", "stars")],
        row.names = FALSE)

  cat("\nModel Fit:\n")
  if (!is.null(object$fit$pseudo_r2)) {
    cat("  Pseudo R-squared: ", formatC(object$fit$pseudo_r2, digits = 3, format = "f"), "\n", sep = "")
  }
  if (!is.null(object$fit$AIC)) {
    cat("  Mean AIC: ", formatC(object$fit$AIC, digits = 0, format = "f"), "\n", sep = "")
  }

  cat("\nCall:\n")
  print(object$call)
  invisible(object)
}

#' @export
print.scf_logit <- function(x, digits = 4, ...) {
  cat("Logistic Regression Results (Multiply-Imputed SCF)\n")
  cat("--------------------------------------------------\n")

  df <- x$results
  scale <- attr(df, "scale")
  est_label <- if (!is.null(scale) && scale == "odds") "Odds Ratio" else "Log-Odds"

  pval <- df$p.value
  num_cols <- sapply(df, is.numeric)
  df[num_cols] <- lapply(df[num_cols], round, digits)

  if (all(c("t.value", "p.value") %in% names(df))) {
    df$p.value <- format.pval(pval, digits = digits)
    print(df[, c("term", "estimate", "std.error", "t.value", "p.value", "stars")], row.names = FALSE)
  } else {
    print(df[, c("term", "estimate", "std.error")], row.names = FALSE)
  }

  cat("\nModel Fit Diagnostics:\n")
  if (!is.null(x$fit$pseudo_r2)) {
    cat("  Pseudo R-squared: ", round(x$fit$pseudo_r2, digits), "\n")
  }
  if (!is.null(x$fit$AIC)) {
    cat("  Mean AIC:         ", round(x$fit$AIC, digits), "\n")
  }

  cat("\nNotes:\n")
  cat(" - Estimates are reported on the", est_label, "scale.\n")
  cat(" - Implicate-level models are stored in `object$imps`\n")
  invisible(x)
}
