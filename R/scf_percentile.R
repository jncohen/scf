#' Estimate Percentiles in SCF Microdata
#'
#' This function estimates a weighted percentile of a continuous variable 
#' in the Survey of Consumer Finances (SCF). It reproduces the procedure used 
#' in the Federal Reserve Board's published SCF Bulletin SAS macro for 
#' distributional statistics (Federal Reserve Board 2023c). This convention is 
#' specific to SCF descriptive distributional statistics (quantiles, 
#' proportions) and differs from standard handling (i.e., using Rubin's Rule).
#'
#' The operation to render the estimates
#' 1. For each implicate of the Survey, it estimates the requested percentile 
#'    using Lumley *et al.*'s [survey::svyquantile()] function:
#'    [survey::svyquantile()] and `se = TRUE`.
#'
#' 2. The reported point estimate for this statistic is the mean of those 
#'    M implicate-specific percentile estimates.
#'
#' 3. The standard error follows the SCF Bulletin SAS macro convention.
#'    The total variance is:
#'
#'        V_total = V1 + ((M + 1) / M) * B
#'
#'    where:
#'    - V1 is the replicate-weight sampling variance of the percentile
#'      from the first implicate only.
#'    - B  is the between-implicate variance of the percentile estimates.
#'
#'    The reported standard error is sqrt(V_total).
#'
#' 4. If a grouping variable is supplied, the same logic is applied
#'    separately within each group.
#'
#' @param scf A `scf_mi_survey` object created with [scf_load()]. Must
#'   contain the list of replicate-weighted designs for each implicate in
#'   `scf$mi_design`.
#' @param var A one-sided formula naming the continuous variable to
#'   summarize (for example `~networth`).
#' @param q Numeric percentile in 0 to 1. Default 0.5 (median).
#' @param by Optional one-sided formula naming a categorical grouping
#'   variable. If supplied, the percentile is estimated separately within
#'   each group.
#' @param verbose Logical. If TRUE, include implicate-level estimates in
#'   the returned object for inspection. Default FALSE.
#'
#' @return An object of class "scf_percentile" with:
#'   - results: data frame of pooled percentile estimates, pooled
#'     standard errors, and implicate min/max. One row per group or one
#'     row total.
#'   - imps: list of implicate-level estimates and standard errors.
#'   - aux: list with variable name, optional group variable name, and
#'     quantile requested.
#'   - verbose: the `verbose` flag.
#'
#' @references
#' Federal Reserve Board. 2023c. "SAS Macro: Variable Definitions."
#' https://www.federalreserve.gov/econres/files/bulletin.macro.txt
#'
#' @seealso [scf_median()], [scf_mean()]
#'
#' @export
scf_percentile <- function(scf, var, q = 0.5, by = NULL, verbose = FALSE) {
  
  # Warn if mock data is being used
  if (isTRUE(attr(scf, "mock"))) {
    warning(
      "Mock data detected. Do not interpret results as valid SCF estimates.",
      call. = FALSE
    )
  }
  
  # Extract variable names from formulas using base R
  varname <- all.vars(var)[1]
  byname  <- if (!is.null(by)) all.vars(by)[1] else NULL
  
  # List of replicate-weighted survey designs (one per implicate)
  # Please an object storing the number of implicates used
  designs <- scf$mi_design
  M       <- length(designs)
  
  # If grouped, coerce grouping variable in each implicate to have
  # identical factor levels for consistent categories. This is for the 
  # contingency that at least one, but not all, implicates registers zero 
  # observations in one category of a multichotomous variable.
  if (!is.null(byname)) {
    for (i in seq_len(M)) {
      designs[[i]]$variables[[byname]] <-
        factor(designs[[i]]$variables[[byname]])
    }
    groups <- levels(designs[[1]]$variables[[byname]])
  }
  
  # Internal utility to estimate percentile in one survey design,
  # optionally within one group.
  get_quantile_obj <- function(dsgn, vname, qval, g = NULL, gname = NULL) {
    if (!is.null(g)) {
      # subset with survey::subset() to preserve replicate structure
      dsgn <- survey::subset(dsgn, dsgn$variables[[gname]] == g)
    }
    survey::svyquantile(
      stats::as.formula(paste0("~", vname)),
      dsgn,
      quantiles     = qval,
      se            = TRUE,
      interval.type = "quantile"
    )
  }
  
  if (is.null(byname)) {
    # -------------------------------------------------
    # Ungrouped case (whole population)
    # -------------------------------------------------
    
    # Step 1. Estimate percentile for each implicate
    imp_objs <- lapply(seq_len(M), function(i) {
      get_quantile_obj(designs[[i]], varname, q)
    })
    
    # Step 2. Build implicate-level table for auditing / verbose output
    imp_estimates <- lapply(seq_len(M), function(i) {
      obj_i <- imp_objs[[i]]
      data.frame(
        implicate = i,
        group     = "All",
        quantile  = q,
        estimate  = as.numeric(stats::coef(obj_i)),
        se        = sqrt(diag(stats::vcov(obj_i))),
        stringsAsFactors = FALSE
      )
    })
    
    # Step 3. Vector of percentile estimates across implicates
    point_vec <- sapply(imp_objs, function(o) as.numeric(stats::coef(o)))
    
    # Step 4. Pooled point estimate = mean across implicates
    qbar <- mean(point_vec)
    
    # Step 5. V1 = replicate-weight sampling variance from first implicate
    V1 <- stats::vcov(imp_objs[[1]])  # 1x1 matrix
    
    # Step 6. Between-implicate variance of point estimates
    B  <- stats::var(point_vec)       # scalar
    
    # Step 7. Total variance per SCF Bulletin SAS macro
    # V_total = V1 + ((M + 1) / M) * B
    V_total   <- as.numeric(V1) + ((M + 1) / M) * B
    se_pooled <- sqrt(V_total)
    
    # Step 8. Output row
    out_df <- data.frame(
      variable = varname,
      quantile = q,
      estimate = qbar,
      se       = se_pooled,
      min      = min(point_vec),
      max      = max(point_vec),
      stringsAsFactors = FALSE
    )
    
  } else {
    # -------------------------------------------------
    # Grouped case (within each category of `by`)
    # -------------------------------------------------
    
    # For each implicate and each group, get that group's percentile
    # imp_objs[[i]][[j]] is svyquantile result for implicate i, group j
    imp_objs <- lapply(seq_len(M), function(i) {
      dsgn_i <- designs[[i]]
      lapply(groups, function(g) {
        get_quantile_obj(dsgn_i, varname, q, g = g, gname = byname)
      })
    })
    
    # Implicate-level table for auditing / verbose output
    imp_estimates <- lapply(seq_len(M), function(i) {
      objs_i <- imp_objs[[i]]
      do.call(rbind, lapply(seq_along(groups), function(j) {
        obj_ij <- objs_i[[j]]
        data.frame(
          implicate = i,
          group     = groups[j],
          quantile  = q,
          estimate  = as.numeric(stats::coef(obj_ij)),
          se        = sqrt(diag(stats::vcov(obj_ij))),
          stringsAsFactors = FALSE
        )
      }))
    })
    
    # Pool per group using the SCF Bulletin variance formula
    out_df <- do.call(rbind, lapply(seq_along(groups), function(j) {
      
      # Collect this group's objects across implicates
      group_objs <- lapply(seq_len(M), function(i) imp_objs[[i]][[j]])
      
      # Vector of percentile estimates for this group across implicates
      point_vec <- sapply(group_objs, function(o) as.numeric(stats::coef(o)))
      
      # Mean percentile for this group
      qbar_g <- mean(point_vec)
      
      # V1_g from implicate 1 for this group
      V1_g <- stats::vcov(group_objs[[1]])
      
      # Between-implicate variance across implicates for this group
      B_g <- stats::var(point_vec)
      
      # Total variance per SCF Bulletin SAS macro
      V_total_g   <- as.numeric(V1_g) + ((M + 1) / M) * B_g
      se_pooled_g <- sqrt(V_total_g)
      
      data.frame(
        group    = groups[j],
        variable = varname,
        quantile = q,
        estimate = qbar_g,
        se       = se_pooled_g,
        min      = min(point_vec),
        max      = max(point_vec),
        stringsAsFactors = FALSE
      )
    }))
    
    # Reorder columns for readability
    out_df <- out_df[, c("group", "variable", "quantile",
                         "estimate", "se", "min", "max")]
  }
  
  # Final return object
  names(imp_estimates) <- paste0("imp", seq_len(M))
  
  structure(
    list(
      results = out_df,
      imps    = imp_estimates,
      aux     = list(
        varname  = varname,
        byname   = byname,
        quantile = q
      ),
      verbose = verbose
    ),
    class = "scf_percentile"
  )
}

#' @export
print.scf_percentile <- function(x, ...) {
  cat("SCF Percentile Estimate (SCF Bulletin convention)\n\n")
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
  cat("Summary of SCF Percentile Estimate\n\n")
  cat("Pooled Estimates (SCF Bulletin convention):\n")
  print(format(object$results, digits = 4, nsmall = 2),
        row.names = FALSE, ...)
  cat("\nImplicate-Level Estimates:\n")
  imp_df <- do.call(rbind, object$imps)
  print(format(imp_df, digits = 4, nsmall = 2),
        row.names = FALSE)
  invisible(object)
}
