#' @importFrom stats formula residuals coef vcov AIC predict family confint qt
NULL

#' S3 Methods for scf_model_result Objects
#'
#' @description
#' Generic S3 methods dispatched on objects of class `"scf_model_result"`,
#' as returned by [scf_ols()], [scf_glm()],
#' [scf_logit()], and [scf_quantreg()]. Use them as you
#' would on any fitted model in R.
#'
#' \describe{
#'   \item{\code{coef()}}{Pooled coefficient estimates (Rubin's Rules).}
#'   \item{\code{vcov()}}{Pooled variance-covariance matrix. For
#'     `scf_logit` it is on the log-odds scale, even when the estimates
#'     are odds ratios.}
#'   \item{\code{confint()}}{Confidence intervals from the t distribution
#'     with Rubin's degrees of freedom. For odds ratios, the interval is
#'     computed on the log-odds scale and then exponentiated.}
#'   \item{\code{AIC()}}{Mean AIC across implicates. Compare it only across
#'     models fit to the same outcome and data; lower is better.}
#'   \item{\code{residuals()}}{Residuals from the first implicate model (diagnostic use only).}
#'   \item{\code{predict()}}{Mean predictions pooled across all five implicate models.}
#'   \item{\code{formula()}}{The model formula.}
#' }
#'
#' @param object An object of class `"scf_model_result"`.
#' @param x An object of class `"scf_model_result"` (for `formula`).
#' @param k Not used. The AIC is the mean of the implicate AICs, with the
#'   usual penalty of 2.
#' @param newdata Optional data frame of new observations for `predict()`.
#'   If missing, predictions are made on the original training data.
#' @param parm Coefficients to include, by name or position. Defaults to all.
#' @param level Confidence level for `confint()`. Defaults to 0.95.
#' @param type Prediction scale for `predict()`: `"link"` (default)
#'   or `"response"`. For a logistic model, `"response"` gives
#'   predicted probabilities.
#' @param ... Additional arguments (not used by most methods).
#'
#' @name scf_model_result_methods
#' @aliases coef.scf_model_result vcov.scf_model_result AIC.scf_model_result
#'   residuals.scf_model_result predict.scf_model_result formula.scf_model_result
#'   confint.scf_model_result
NULL

.scf_imp_models <- function(object) {
  if (!is.null(object$models) && length(object$models) > 0) {
    return(object$models)
  }
  if (!is.null(object$imps) && length(object$imps) > 0) {
    return(object$imps)
  }
  NULL
}

#' @rdname scf_model_result_methods
#' @export
formula.scf_model_result <- function(x, ...) {
  if (is.null(x$formula)) {
    stop("Formula not found in the model object. Ensure the model function saves the formula argument.")
  }
  x$formula
}

#' @rdname scf_model_result_methods
#' @export
residuals.scf_model_result <- function(object, ...) {
  imp_models <- .scf_imp_models(object)
  if (is.null(imp_models)) {
    stop("No underlying models found to extract residuals.")
  }
  stats::residuals(imp_models[[1]])
}

#' @rdname scf_model_result_methods
#' @export
coef.scf_model_result <- function(object, ...) {
  est <- object$results$estimate
  names(est) <- object$results$term
  est
}

#' @rdname scf_model_result_methods
#' @export
vcov.scf_model_result <- function(object, ...) {
  if (is.null(object$vcov)) {
    stop("Pooled variance-covariance matrix not found. Re-fit the model with the current version of the package.")
  }
  object$vcov
}

#' @rdname scf_model_result_methods
#' @export
AIC.scf_model_result <- function(object, k = 2, ...) {
  if (is.null(object$fit$AIC) || is.na(object$fit$AIC)) {
    stop("AIC not available in model diagnostics ('fit$AIC').")
  }
  object$fit$AIC
}

#' @rdname scf_model_result_methods
#' @export
predict.scf_model_result <- function(object, newdata, type = "link", ...) {
  imp_models <- .scf_imp_models(object)
  if (is.null(imp_models)) {
    stop("No underlying models found to generate predictions.")
  }

  pred_type <- match.arg(type, c("link", "response"))
  has_newdata <- !missing(newdata)

  all_preds <- lapply(imp_models, function(m) {
    p <- tryCatch({
      if (inherits(m, "rq")) {
        if (has_newdata) stats::predict(m, newdata = newdata, ...) else stats::fitted(m)
      } else if (has_newdata) {
        stats::predict.glm(m, newdata = newdata, type = pred_type, ...)
      } else {
        stats::predict.glm(m, type = pred_type, ...)
      }
    }, error = function(e) {
      stop(paste("Prediction failed for an underlying model. Error:", conditionMessage(e)))
    })
    as.vector(p)
  })

  if (length(unique(sapply(all_preds, length))) > 1) {
    stop("Prediction lengths are inconsistent across implicates. Check 'newdata'.")
  }

  pred_matrix <- do.call(cbind, all_preds)
  rowMeans(pred_matrix, na.rm = TRUE)
}

#' @rdname scf_model_result_methods
#' @export
confint.scf_model_result <- function(object, parm, level = 0.95, ...) {
  est <- object$results$estimate
  se <- object$results$std.error
  df <- if (is.null(object$df)) Inf else object$df
  q <- stats::qt(1 - (1 - level) / 2, df)

  if (identical(attr(object$results, "scale"), "odds")) {
    b <- log(est)
    s <- se / est
    lims <- cbind(exp(b - q * s), exp(b + q * s))
  } else {
    lims <- cbind(est - q * se, est + q * se)
  }

  pct <- paste(format(100 * c((1 - level) / 2, 1 - (1 - level) / 2), trim = TRUE), "%")
  dimnames(lims) <- list(object$results$term, pct)
  if (!missing(parm)) lims <- lims[parm, , drop = FALSE]
  lims
}
