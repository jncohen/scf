#' Estimate Logistic Regression Model using SCF Microdata
#'
#' @description
#' Fits a replicate-weighted logistic regression model to multiply-imputed SCF data,
#' returning pooled coefficients or odds ratios with model diagnostics. Use this
#' function to model a binary variable as a function of predictors.
#'
#' @section Details:
#' This function internally calls `scf_glm()` with `family = binomial()` and optionally
#' exponentiates pooled log-odds to odds ratios.
#'
#' Logistic regression models the probability of a binary outcome using the
#' logit link.
#'
#' Coefficients reflect the change in log-odds associated with a one-unit change
#' in the predictor. 
#'
#' When `odds = TRUE`, the coefficient estimates and standard errors are
#' transformed from log-odds to odds ratios and approximate SEs.
#'
#' @param object A `scf_mi_survey` object created with [scf_load()] and [scf_design()].
#' @param formula A model formula specifying a binary outcome and predictors, e.g., `rich ~ age + factor(edcl)`.
#' @param odds Logical. If `TRUE` (default), exponentiates coefficient estimates to produce odds ratios for interpretability.
#' @param ... Additional arguments passed to [scf_glm()].
#'
#' @return An object of class `"scf_logit"` and `"scf_model_result"` with:
#' \describe{
#'   \item{results}{A data frame of pooled estimates (log-odds or odds ratios), standard errors, and test statistics.}
#'   \item{fit}{Model diagnostics including AIC and pseudo-R-Squared (for binomial family).}
#'   \item{models}{List of implicate-level `svyglm` model objects.}
#'   \item{call}{The matched function call.}
#' }
#'
#' @examples
#' \donttest{
#' # Load bundled mock data (for demonstration only — not real SCF data)
#' scf2022 <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))

#' # For real analysis, use:
#' # scf_download(2022); scf2022 <- scf_load(2022)
#'
#' scf2022 <- scf_update(scf2022, rich = as.integer(networth > 1e6))
#' model <- scf_logit(scf2022, rich ~ age + factor(edcl), odds = TRUE)
#' print(model)
#' }
#'
#' @section Warning:
#' When modeling binary outcomes using survey-weighted logistic regression,
#' users may encounter the warning:
#'
#'   `"non-integer #successes in a binomial glm!"`
#'
#' This message is benign. It results from replicate-weighted survey designs
#' where the implied number of "successes" is non-integer. The model is
#' estimated correctly. Coefficients are valid and consistent with
#' maximum likelihood.
#'
#' For background, see:
#' https://stackoverflow.com/questions/12953045/warning-non-integer-successes-in-a-binomial-glm-survey-packages
#'
#' @seealso [scf_glm()], [scf_ols()], [scf_MIcombine()]
#'
#' @export
scf_logit <- function(object, formula, odds = TRUE, ...) {
  model <- scf_glm(object, formula, family = binomial(), ...)

  if (isTRUE(attr(object, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }


  if (odds) {
    logit_est  <- model$results$estimate
    logit_se   <- model$results$std.error
    zval       <- logit_est / logit_se
    pval       <- 2 * pnorm(-abs(zval))
    stars      <- cut(pval,
                      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.10, Inf),
                      labels = c("***", "**", "*", ".", ""),
                      right = FALSE)
    
    model$results$estimate   <- exp(logit_est)
    model$results$std.error  <- model$results$estimate * logit_se
    model$results$t.value    <- zval
    model$results$p.value    <- pval
    model$results$stars      <- stars
    attr(model$results, "scale") <- "odds"
  } else {
    attr(model$results, "scale") <- "logit"
  }

  out <- list(
    results = model$results,
    imps = model$models,
    fit = model$fit,
    call = match.call()
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

  num_cols <- sapply(df, is.numeric)
  df[num_cols] <- lapply(df[num_cols], round, digits)

  if (all(c("t.value", "p.value") %in% names(df))) {
    df$p.value <- format.pval(df$p.value, digits = digits)
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
