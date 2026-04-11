#' @importFrom stats formula residuals coef vcov AIC predict family
NULL 

#' Generic S3 Method: formula.scf_model_result
#'
#' Extracts the formula used to fit a multiply-imputed SCF regression model.
#'
#' @param x An object of class 'scf_model_result'.
#' @param ... Not used.
#' @return A model formula object.
#' @export
formula.scf_model_result <- function(x, ...) {
  if (is.null(x$formula)) {
    stop("Formula not found in the model object. Ensure the model function saves the formula argument.")
  }
  return(x$formula)
}

# ----------------------------------------------------------------------

#' Generic S3 Method: residuals.scf_model_result
#'
#' Extracts the residuals vector from the first underlying implicate model.
#'
#' In multiply-imputed data, pooled residuals are typically not calculated.
#' This returns the residuals from the primary (first) implicate model, 
#' which is suitable for simple diagnostics.
#'
#' @param object An object of class 'scf_model_result'.
#' @param ... Not used.
#' @return A numeric vector of residuals.
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

#' Generic S3 Method: coef.scf_model_result
#'
#' Extracts the pooled coefficient estimates from the model result.
#'
#' @param object An object of class 'scf_model_result'.
#' @param ... Not used.
#' @return A named numeric vector of pooled coefficient estimates.
#' @export
coef.scf_model_result <- function(object, ...) {
  # Estimates are stored in the results data frame
  est <- object$results$estimate
  names(est) <- object$results$term
  return(est)
}

# ----------------------------------------------------------------------

#' Generic S3 Method: vcov.scf_model_result
#'
#' Reconstructs the pooled variance-covariance matrix of the model coefficients.
#'
#' NOTE: The pooled variance matrix is NOT stored directly, only the coefficients
#' and SEs. This method is typically skipped in favour of direct SE access or 
#' custom pooling, but is included here to provide a complete S3 interface.
#'
#' @param object An object of class 'scf_model_result'.
#' @param ... Not used.
#' @return Returns the variance-covariance matrix from the internal pooling object if available, 
#' or stops with an error if the model object doesn't retain the raw pooled variance.
#' @export
vcov.scf_model_result <- function(object, ...) {
  # Since the raw pooling object is not explicitly exposed, 
  # we stop with a message directing users to the SEs, or, ideally,
  # the model functions (scf_ols/glm) should save the 'pooled' object 
  # created by scf_MIcombine(). Assuming the current structure:
  stop("Pooled variance-covariance matrix is not stored in this object. Please use SE() to extract pooled standard errors.")
}

# ----------------------------------------------------------------------

#' Generic S3 Method: AIC.scf_model_result
#'
#' Extracts the mean of the Akaike Information Criterion (AIC) across implicates.
#'
#' @param object An object of class 'scf_model_result'.
#' @param k The penalty term (2 for AIC, log(n) for BIC). Defaults to 2.
#' @param ... Not used.
#' @return The numeric mean AIC pooled across implicates.
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

#' Generic S3 Method: predict.scf_model_result
#'
#' Calculates predicted values for a new data set (or the original data) 
#' by pooling predictions across all multiply-imputed models.
#'
#' @param object An object of class 'scf_model_result'.
#' @param newdata A data frame containing variables for which to predict. 
#'   If missing, predictions are made on the original data (from the first implicate).
#' @param type Character string specifying the type of prediction. 
#'   Options are "link" (default, linear predictor) or "response" (fitted values on the outcome scale).
#' @param ... Additional arguments passed to predict.glm.
#' @return A numeric vector of pooled predicted values (mean prediction across implicates).
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
