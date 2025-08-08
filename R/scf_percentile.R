#' Estimate Percentile in a Continuous Variable in SCF Microdata
#'
#' @description
#' Calculates the percentile score of a continuous variable in the SCF microdata.
#' Use this function to either (1) identify where a continuous variable's
#' value stands in relation to all observed values, or (2) to discern value
#' below which a user-specified percentage of households fall on
#' that metric.
#'
#' @section Details:
#' The percentile is a value below which a given percentage of observations
#' fall.  This function estimates the desired percentile score within each
#' implicate of the SCF’s  multiply-imputed dataset, and then averages them to
#' generate a population estimate.
#'
#' When a grouping variable is supplied, the percentile is estimated separately
#' within each group in each implicate. Group-level results are then pooled
#' across implicates.
#'
#' Unlike [scf_mean()], this function does not pool results using Rubin’s Rules.
#' Instead, it follows the Federal Reserve’s practice for reporting percentiles
#' in official SCF publications: compute the desired percentile separately
#' within each implicate, then average the resulting values to obtain a pooled
#' estimate.
#'
#' Standard errors are approximated using the sample standard deviation of the
#' five implicate-level estimates. This method is consistent with the SCF's
#' official percentile macro (see Kennickell 1998; per Federal Reserve Board's
#' 2022 SCF's official SAS script)
#'
#' @param scf A scf_mi_survey object created with [scf_load()]. Must contain five implicates.
#' @param var A one-sided formula identifying the continuous variable to summarize (e.g., ~networth).
#' @param q A quantile to estimate (between 0 and 1). Defaults to 0.5 (median).
#' @param by Optional one-sided formula specifying a discrete grouping variable for stratified percentiles.
#' @param verbose Logical. If TRUE, include implicate-level results in print output. Default is FALSE.
#'
#' @return A list of class "scf_percentile" with:
#' \describe{
#'   \item{results}{Pooled percentile estimates with standard errors and range across implicates. One row per group, or one row total.}
#'   \item{imps}{A named list of implicate-level estimates.}
#'   \item{aux}{Variable, group, and quantile metadata.}
#' }
#'
#' @examples
#' \donttest{
#' # Load bundled mock data (for demonstration only — not real SCF data)
#' scf2022 <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))

#' # For real analysis, use:
#' # scf_download(2022); scf2022 <- scf_load(2022)
#'
#' scf_percentile(scf2022, ~networth, q = 0.5)
#' scf_percentile(scf2022, ~networth, q = 0.9, by = ~edcl)
#' }
#'
#' @seealso [scf_median()]
#'
#' @references
#' Kennickell, Arthur B. (2017). "Multiple Imputation in the Survey of Consumer Finances." *Statistical Journal of the IAOS*, 33(1).
#'
#' U.S. Federal Reserve (2023) *Codebook for 2022 Survey of Consumer Finances* <https://www.federalreserve.gov/econres/files/codebk2022.txt>
#'
#' @export
scf_percentile <- function(scf, var, q = 0.5, by = NULL, verbose = FALSE) {

  if (isTRUE(attr(scf, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }


  varname <- all.vars(var)[1]
  byname <- if (!is.null(by)) all.vars(by)[1] else NULL
  nimp <- length(scf$mi_design)
  imp_results <- vector("list", nimp)
  
  for (i in seq_len(nimp)) {
    df <- scf$mi_design[[i]]$variables
    
    weights <- df$wgt
    values <- df[[varname]]
    if (is.null(byname)) {
      imp_results[[i]] <- data.frame(
        implicate = i,
        group = "All",
        estimate = weighted_quantile(values, weights, q),
        stringsAsFactors = FALSE
      )
    } else {
      df[[byname]] <- as.factor(df[[byname]])
      groups <- levels(df[[byname]])
      imp_results[[i]] <- do.call(rbind, lapply(groups, function(g) {
        idx <- which(df[[byname]] == g & !is.na(values))
        if (length(idx) < 1) return(NULL)
        data.frame(
          implicate = i,
          group = g,
          estimate = weighted_quantile(values[idx], weights[idx], q),
          stringsAsFactors = FALSE
        )
      }))
    }
  }
  names(imp_results) <- paste0("imp", seq_len(nimp))
  df <- do.call(rbind, imp_results)

  if (is.null(byname)) {
    x <- df$estimate
    out <- data.frame(
      variable = varname,
      quantile = q,
      estimate = mean(x),
      se = sd(x),
      min = min(x),
      max = max(x),
      stringsAsFactors = FALSE
    )
  } else {
    group_levels <- sort(unique(df$group))
    out <- do.call(rbind, lapply(group_levels, function(g) {
      x <- df$estimate[df$group == g]
      data.frame(
        group = g,
        variable = varname,
        quantile = q,
        estimate = mean(x),
        se = sd(x),
        min = min(x),
        max = max(x),
        stringsAsFactors = FALSE
      )
    }))
    out <- out[, c("group", "variable", "quantile", "estimate", "se", "min", "max")]
  }

  structure(list(
    results = out,
    imps = imp_results,
    aux = list(varname = varname, byname = byname, quantile = q),
    verbose = verbose
  ), class = "scf_percentile")
}

#' @export
print.scf_percentile <- function(x, ...) {
  cat("Multiply-Imputed Percentile Estimate\n\n")
  print(x$results, row.names = FALSE, ...)
  if (isTRUE(x$verbose)) {
    cat("\nImplicate-Level Estimates:\n\n")
    imp_df <- do.call(rbind, x$imps)
    print(imp_df, row.names = FALSE)
  }
  invisible(x)
}

#' @export
summary.scf_percentile <- function(object, ...) {
  cat("Summary of Multiply-Imputed Percentile Estimate\n\n")
  cat("Pooled Estimates:\n")
  print(format(object$results, digits = 4, nsmall = 2), row.names = FALSE, ...)
  cat("\nImplicate-Level Estimates:\n")
  imp_df <- do.call(rbind, object$imps)
  print(format(imp_df, digits = 4, nsmall = 2), row.names = FALSE)
  invisible(object)
}

# Weighted quantile: SCF-defined (non-interpolated threshold)
weighted_quantile <- function(x, w, q) {
  keep <- !is.na(x) & !is.na(w)
  x <- x[keep]
  w <- w[keep]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  wcum <- cumsum(w) / sum(w)
  x[which(wcum >= q)[1]]
}
