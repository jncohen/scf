#' Estimate Mean in Multiply-Imputed SCF Data
#'
#' @description
#' Returns the population-level estimate of a continuous variable's weighted
#' mean across the Survey's five implicates. Use this operation to derive an
#' estimate of a population's 'typical' or 'average' score on a continuous
#' variable.
#'
#' @section Details:
#' The mean is a measure of central tendency that represents the arithmetic average of a distribution.
#' It is most appropriate when the distribution is symmetric and not heavily skewed.
#' Unlike the median, the mean is sensitive to extreme values, which may distort interpretation in the presence of outliers.
#' Use this function to describe the “typical” value of a continuous variable in the population or within subgroups.
#'
#' @param scf A scf_mi_survey object created with [scf_load()]. Must contain five replicate-weighted implicates.
#' @param var A one-sided formula identifying the continuous variable to summarize (e.g., ~networth).
#' @param by Optional one-sided formula specifying a discrete grouping variable for stratified means.
#' @param verbose Logical. If TRUE, include implicate-level results in print output. Default is FALSE.
#'
#' @return A list of class "scf_mean" with:
#' \describe{
#'   \item{results}{Pooled estimates with standard errors and range across implicates. One row per group, or one row total.}
#'   \item{imps}{A named list of implicate-level estimates.}
#'   \item{aux}{Variable and group metadata.} 
#' }
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td  <- tempdir()
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Estimate means
#' scf_mean(scf2022, ~networth)
#' scf_mean(scf2022, ~networth, by = ~edcl)
#' 
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(file.path(td, "scf2022.rds"), force = TRUE)
#'
#'
#' @seealso [scf_median()], [scf_percentile()], [scf_xtab()], [scf_plot_dist()]
#'
#' @export
scf_mean <- function(scf, var, by = NULL, verbose = FALSE) {
  
  if (!inherits(scf, "scf_mi_survey") ||
      !is.list(scf$mi_design) ||
      !all(sapply(scf$mi_design, inherits, "svyrep.design"))) {
    stop("Input must be a 'scf_mi_survey' object with replicate-weighted implicates.")
  }
  
  if (isTRUE(attr(scf, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }
  
  varname <- all.vars(var)[1]
  byname  <- if (!is.null(by)) all.vars(by)[1] else NULL
  designs <- scf$mi_design
  nimp    <- length(designs)
  
  if (is.null(byname)) {
    
    imp_results <- lapply(seq_len(nimp), function(i) {
      d <- designs[[i]]
      survey::svymean(var, d)
    })
    
    imp_estimates <- lapply(seq_len(nimp), function(i) {
      est <- imp_results[[i]]
      data.frame(
        implicate = i,
        group     = "All",
        estimate  = as.numeric(coef(est)),
        se        = sqrt(diag(vcov(est))),
        stringsAsFactors = FALSE
      )
    })
    
    pooled <- scf_MIcombine(imp_results)
    
    est_pooled <- as.numeric(pooled$coefficients)
    se_pooled  <- sqrt(diag(pooled$variance))
    
    x_all <- sapply(imp_results, function(obj) as.numeric(coef(obj)))
    
    out <- data.frame(
      variable = varname,
      estimate = est_pooled,
      se       = se_pooled,
      min      = min(x_all),
      max      = max(x_all),
      stringsAsFactors = FALSE
    )
    
  } else {
    
    for (i in seq_len(nimp)) {
      designs[[i]]$variables[[byname]] <- factor(designs[[i]]$variables[[byname]])
    }
    groups <- levels(designs[[1]]$variables[[byname]])
    
    imp_results <- lapply(seq_len(nimp), function(i) {
      d <- designs[[i]]
      lapply(groups, function(g) {
        d_g <- subset(d, d$variables[[byname]] == g)
        survey::svymean(var, d_g)
      })
    })
    
    imp_estimates <- lapply(seq_len(nimp), function(i) {
      res_i <- imp_results[[i]]
      do.call(rbind, lapply(seq_along(groups), function(j) {
        est <- res_i[[j]]
        data.frame(
          implicate = i,
          group     = groups[j],
          estimate  = as.numeric(coef(est)),
          se        = sqrt(diag(vcov(est))),
          stringsAsFactors = FALSE
        )
      }))
    })
    
    out <- do.call(rbind, lapply(seq_along(groups), function(j) {
      g <- groups[j]
      
      res_list_g <- lapply(seq_len(nimp), function(i) {
        imp_results[[i]][[j]]
      })
      
      pooled_g <- scf_MIcombine(res_list_g)
      
      est_pooled_g <- as.numeric(pooled_g$coefficients)
      se_pooled_g  <- sqrt(diag(pooled_g$variance))
      
      x_g <- sapply(res_list_g, function(obj) as.numeric(coef(obj)))
      
      data.frame(
        group    = g,
        variable = varname,
        estimate = est_pooled_g,
        se       = se_pooled_g,
        min      = min(x_g),
        max      = max(x_g),
        stringsAsFactors = FALSE
      )
    }))
    
    out <- out[, c("group", "variable", "estimate", "se", "min", "max")]
  }
  
  names(imp_estimates) <- paste0("imp", seq_len(nimp))
  
  out_obj <- list(
    results = out,
    imps    = imp_estimates,
    aux     = list(varname = varname, byname = byname),
    verbose = verbose
  )
  class(out_obj) <- "scf_mean"
  return(out_obj)
}

#' @export
print.scf_mean <- function(x, ...) {
  cat("Multiply-Imputed, Replicate-Weighted Mean Estimate\n\n")
  print(x$results, row.names = FALSE, ...)

  if (isTRUE(x$verbose)) {
    cat("\nImplicate-Level Estimates:\n\n")
    imp_df <- do.call(rbind, x$imps)
    print(imp_df, row.names = FALSE)
  }

  invisible(x)
}
#' @export
summary.scf_mean <- function(object, ...) {
  cat("Summary of Multiply-Imputed Mean Estimate\n\n")

  # Print pooled estimates with standard errors
  cat("Pooled Estimates:\n")
  print(
    format(object$results, digits = 4, nsmall = 2),
    row.names = FALSE,
    ...
  )

  # Optionally print implicate-level estimates
  cat("\nImplicate-Level Estimates:\n")
  imp_df <- do.call(rbind, object$imps)
  print(
    format(imp_df, digits = 4, nsmall = 2),
    row.names = FALSE
  )

  invisible(object)
}
