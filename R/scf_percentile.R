#' Estimate Percentiles in SCF Microdata
#'
#' This function estimates a weighted percentile of a continuous variable 
#' in the Survey of Consumer Finances (SCF). It reproduces the procedure used 
#' in the Federal Reserve Board's published SCF Bulletin SAS macro for 
#' distributional statistics (Federal Reserve Board 2023c). This convention is 
#' specific to SCF descriptive distributional statistics (quantiles, 
#' proportions) and differs from standard handling (i.e., using Rubin's Rule).
#'
#' The operation to render the estimates:
#' 1. For each implicate, estimate the requested percentile using
#'    [survey::svyquantile()] with `se = TRUE`.
#' 2. The reported point estimate is the mean of the M implicate-specific
#'    percentile estimates.
#' 3. The standard error follows the SCF Bulletin SAS macro convention:
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
#' @param q Numeric percentile in between 0 and 1. Default 0.5 (median).
#' @param by Optional one-sided formula naming a categorical grouping
#'   variable. If supplied, the percentile is estimated separately within
#'   each group.
#' @param verbose Logical. If TRUE, include implicate-level estimates in
#'   the returned object for inspection. Default FALSE.
#'
#' @return An object of class `"scf_percentile"` containing:
#' \describe{
#'   \item{results}{A data frame containing pooled percentile estimates, pooled
#'     standard errors, and implicate min/max values. One row per group (if
#'     `by` is supplied) or one row otherwise.}
#'   \item{imps}{A list of implicate-level percentile estimates and standard errors.}
#'   \item{aux}{A list containing the variable name, optional group variable name,
#'     and the quantile requested.}
#'   \item{verbose}{Logical flag indicating whether implicate-level estimates
#'     should be printed by `print()` or `summary()`.}
#' }
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()` for actual SCF data
#' td  <- tempdir()
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Estimate the 75th percentile of net worth
#' scf_percentile(scf2022, ~networth, q = 0.75)
#'
#' # Estimate the median net worth by ownership group
#' scf_percentile(scf2022, ~networth, q = 0.5, by = ~own)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(file.path(td, "scf2022.rds"), force = TRUE)
#' rm(scf2022)
#'
#' @references
#' Federal Reserve Board. 2023c. "SAS Macro: Variable Definitions."
#' https://www.federalreserve.gov/econres/files/bulletin.macro.txt
#'
#' @seealso [scf_median()], [scf_mean()]
#'
#' @export
scf_percentile <- function(scf, var, q = 0.5, by = NULL, verbose = FALSE) {
  # Basic checks
  if (!inherits(scf, "scf_mi_survey")) {
    stop("`scf` must be an object of class 'scf_mi_survey'.", call. = FALSE)
  }
  if (!is.numeric(q) || length(q) != 1L || q < 0 || q > 1) {
    stop("`q` must be a single numeric value in [0, 1].", call. = FALSE)
  }
  
  # Warn if mock data is being used
  if (isTRUE(attr(scf, "mock"))) {
    warning(
      "Mock data detected. Do not interpret results as valid SCF estimates.",
      call. = FALSE
    )
  }
  
  # Extract variable names from formulas
  varname <- all.vars(var)[1]
  byname  <- if (!is.null(by)) all.vars(by)[1] else NULL
  
  designs <- scf$mi_design
  M       <- length(designs)
  if (M < 1L) {
    stop("`scf$mi_design` must contain at least one implicate design.", call. = FALSE)
  }
  
  # If grouped, harmonize factor levels across implicates
  if (!is.null(byname)) {
    for (i in seq_len(M)) {
      designs[[i]]$variables[[byname]] <- factor(designs[[i]]$variables[[byname]])
    }
    groups <- levels(designs[[1]]$variables[[byname]])
  }
  
  # Internal utility: estimate percentile for one design, optional group subset,
  # with controlled handling of the known replicate-weight warning.
  get_quantile_obj <- function(dsgn, vname, qval, g = NULL, gname = NULL) {
    if (!is.null(g)) {
      dsgn <- subset(dsgn, dsgn$variables[[gname]] == g)
    }
    
    form <- stats::as.formula(paste0("~", vname))
    
    suppressMessages(
      suppressWarnings(
        survey::svyquantile(
          form,
          dsgn,
          quantiles     = qval,
          se            = TRUE,
          interval.type = "quantile"
        )
      )
    )
  }
  
  
  if (is.null(byname)) {
    ## -------------------------------------------------
    ## Ungrouped case (whole population)
    ## -------------------------------------------------
    
    imp_objs <- lapply(seq_len(M), function(i) {
      get_quantile_obj(designs[[i]], varname, q)
    })
    
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
    
    point_vec <- sapply(imp_objs, function(o) as.numeric(stats::coef(o)))
    qbar      <- mean(point_vec)
    
    V1 <- stats::vcov(imp_objs[[1]])      # 1 x 1
    B  <- stats::var(point_vec)          # scalar
    
    V_total   <- as.numeric(V1) + ((M + 1) / M) * B
    se_pooled <- sqrt(V_total)
    
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
    ## -------------------------------------------------
    ## Grouped case (within each category of `by`)
    ## -------------------------------------------------
    
    imp_objs <- lapply(seq_len(M), function(i) {
      dsgn_i <- designs[[i]]
      lapply(groups, function(g) {
        get_quantile_obj(dsgn_i, varname, q, g = g, gname = byname)
      })
    })
    
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
    
    out_df <- do.call(rbind, lapply(seq_along(groups), function(j) {
      group_objs <- lapply(seq_len(M), function(i) imp_objs[[i]][[j]])
      point_vec  <- sapply(group_objs, function(o) as.numeric(stats::coef(o)))
      
      qbar_g <- mean(point_vec)
      V1_g   <- stats::vcov(group_objs[[1]])
      B_g    <- stats::var(point_vec)
      
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
    
    out_df <- out_df[, c("group", "variable", "quantile",
                         "estimate", "se", "min", "max")]
  }
  
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
