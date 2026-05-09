#' @importFrom stats formula residuals coef vcov AIC predict family
NULL

#' S3 Methods for scf_model_result Objects
#'
#' @description
#' Generic S3 methods dispatched on objects of class \code{"scf_model_result"},
#' as returned by \code{\link{scf_ols}}, \code{\link{scf_glm}},
#' \code{\link{scf_logit}}, and \code{\link{scf_quantreg}}.
#'
#' \describe{
#'   \item{\code{coef()}}{Pooled coefficient estimates (Rubin's Rules).}
#'   \item{\code{vcov()}}{Pooled variance-covariance matrix.}
#'   \item{\code{AIC()}}{Mean AIC across implicates.}
#'   \item{\code{residuals()}}{Residuals from the first implicate model (diagnostic use only).}
#'   \item{\code{predict()}}{Mean predictions pooled across all five implicate models.}
#'   \item{\code{formula()}}{The model formula.}
#' }
#'
#' @param object An object of class \code{"scf_model_result"}.
#' @param x An object of class \code{"scf_model_result"} (for \code{formula}).
#' @param k Penalty term passed to \code{AIC()} (default 2 for AIC, \code{log(n)} for BIC).
#' @param newdata Optional data frame of new observations for \code{predict()}.
#'   If missing, predictions are made on the original training data.
#' @param type Prediction scale for \code{predict()}: \code{"link"} (default)
#'   or \code{"response"}.
#' @param ... Additional arguments (not used by most methods).
#'
#' @name scf_model_result_methods
#' @aliases coef.scf_model_result vcov.scf_model_result AIC.scf_model_result
#'   residuals.scf_model_result predict.scf_model_result formula.scf_model_result
NULL

#' @rdname scf_model_result_methods
#' @export
formula.scf_model_result <- function(x, ...) {
  if (is.null(x$formula)) {
    stop("Formula not found in the model object. Ensure the model function saves the formula argument.")
  }
  return(x$formula)
}

# ----------------------------------------------------------------------

#' @rdname scf_model_result_methods
#' @export
residuals.scf_model_result <- function(object, ...) {
  # Implicate models may be stored under $models (scf_glm, scf_quantreg) or
  # $imps (scf_ols, scf_logit). Check both to ensure uniform behavior.
  imp_models <- if (!is.null(object$models) && length(object$models) > 0) {
    object$models
  } else if (!is.null(object$imps) && length(object$imps) > 0) {
    object$imps
  } else {
    NULL
  }
  if (is.null(imp_models)) {
    stop("No underlying models found to extract residuals.")
  }
  return(stats::residuals(imp_models[[1]]))
}

# ----------------------------------------------------------------------

#' @rdname scf_model_result_methods
#' @export
coef.scf_model_result <- function(object, ...) {
  # Estimates are stored in the results data frame
  est <- object$results$estimate
  names(est) <- object$results$term
  return(est)
}

# ----------------------------------------------------------------------

#' @rdname scf_model_result_methods
#' @export
vcov.scf_model_result <- function(object, ...) {
  if (is.null(object$vcov)) {
    stop("Pooled variance-covariance matrix not found. Re-fit the model with the current version of the package.")
  }
  object$vcov
}

# ----------------------------------------------------------------------

#' @rdname scf_model_result_methods
#' @export
AIC.scf_model_result <- function(object, k = 2, ...) {
  if (is.null(object$fit$AIC) || is.na(object$fit$AIC)) {
    stop("AIC not available in model diagnostics ('fit$AIC').")
  }
  # For multiply imputed models, we typically use the mean AIC across implicates.
  # Note: This is an approximation and does not fully conform to MI literature on AIC.
  return(object$fit$AIC) 
}


# ----------------------------------------------------------------------

#' @rdname scf_model_result_methods
#' @export
predict.scf_model_result <- function(object, newdata, type = "link", ...) {

  # Implicate models may be stored under $models (scf_glm, scf_quantreg) or
  # $imps (scf_ols, scf_logit). Resolve once and use throughout.
  imp_models <- if (!is.null(object$models) && length(object$models) > 0) {
    object$models
  } else if (!is.null(object$imps) && length(object$imps) > 0) {
    object$imps
  } else {
    NULL
  }
  if (is.null(imp_models)) {
    stop("No underlying models found to generate predictions.")
  }

  # --- 1. Determine Model Type and Prediction Scale ---

  pred_type <- match.arg(type, c("link", "response"))

  # Try to infer family (necessary for predict.glm to handle links/responses)
  model_family <- tryCatch({
    stats::family(imp_models[[1]])
  }, error = function(e) list(family = "gaussian"))

  # --- 2. Predict on each Implicate Model ---

  all_preds <- lapply(imp_models, function(m) {
    # If no new data is provided, use the original data attached to the model
    local_newdata <- if (missing(newdata)) {
      m$model 
      # Fallback to model.frame if m$model is missing (e.g. for svyglm objects)
      # More reliably, if no newdata is provided, the goal is to predict on original data.
    } else {
      newdata
    }
    
    # If using OLS (gaussian) or a model like Poisson, 'link' and 'response' scale predictions
    # can be directly averaged. For binomial, we almost always average on the 'response' scale.
    
    # Run the prediction using the chosen type
    p <- tryCatch({
      stats::predict(m, newdata = local_newdata, type = pred_type, ...)
    }, error = function(e) {
      stop(paste("Prediction failed for an underlying model. Error:", conditionMessage(e)))
    })
    
    # Convert prediction to a vector if it came back as a matrix or data frame
    as.vector(p)
  })
  
  # --- 3. Pool Predictions ---
  
  if (length(unique(sapply(all_preds, length))) > 1) {
    stop("Prediction lengths are inconsistent across implicates. Check 'newdata'.")
  }
  
  # Stack predictions into a matrix (one column per implicate)
  pred_matrix <- do.call(cbind, all_preds)
  
  # The pooled prediction is simply the mean across all implicates for each observation.
  pooled_prediction <- rowMeans(pred_matrix, na.rm = TRUE)
  
  return(pooled_prediction)
}
