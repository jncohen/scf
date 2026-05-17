#' Summarize SCF Variables by Percentile Group
#'
#' @description
#' Creates a percentile-based grouping variable for a continuous SCF variable
#' and optionally computes a summary statistic within each group. Two methods
#' are available. The \code{"implicate"} method (the default) computes
#' survey-weighted quantile thresholds separately within each implicate, the
#' statistically preferable approach under multiple imputation, as it correctly
#' accounts for between-implicate variation in imputed values. The
#' \code{"stack"} method replicates the Federal Reserve's published convention,
#' in which all five implicates are pooled with weights divided by five,
#' observations are sorted by the percentile variable and household identifier,
#' percentile groups are assigned from the cumulative weighted population share,
#' and a flat weighted statistic is computed directly on the stacked data.
#'
#' @details
#' The two methods will generally produce similar but not identical results.
#' Use \code{method = "stack"} when replication of the Federal Reserve's
#' published percentile-group point estimates is required. Note that 
#' \code{method = "stack"} with \code{stat != "none"} computes a flat weighted 
#' statistic on the pooled stacked data and does not use replicate-weight 
#' survey machinery or Rubin's combining rules. No standard error is 
#' returned in this case.
#'
#' When \code{stat = "none"}, the function returns the input
#' \code{scf_mi_survey} object with the grouping variable added, ready to pass
#' to \code{\link{scf_mean}}, \code{\link{scf_median}}, or other estimation
#' functions via the \code{by} argument. For standard grouping variables
#' already published by the Fed, such as net worth percentile (\code{nwcat})
#' and income percentile (\code{inccat}), those variables are included directly
#' in the data returned by \code{\link{scf_load}} and can be passed to
#' \code{by} without calling \code{scf_pctile_sum}.
#' 
#' For \code{method = "stack"}, percentile groups are assigned using the
#' Federal Reserve SAS macro logic: after stacking implicates and dividing
#' weights by five, observations are sorted by the variable used to define the
#' percentile groups and by household identifier. The cumulative weighted
#' population share is then used to assign groups. This is not equivalent to
#' first estimating a percentile threshold and then applying a simple
#' \code{>} or \code{>=} comparison, especially when observations lie at or
#' near a percentile boundary.
#'
#'
#' @param scf A \code{scf_mi_survey} object created with \code{\link{scf_load}}.
#' @param var A one-sided formula naming the continuous variable to cut
#'   (e.g., \code{~networth}).
#' @param probs Numeric vector with values in between 0 and 1 defining group boundaries,
#'   including 0 and 1 as endpoints. Defaults to deciles
#'   (\code{seq(0, 1, by = 0.1)}).
#' @param labels Optional character vector of group labels, length equal to
#'   \code{length(probs) - 1}. If \code{NULL} (default), labels are generated
#'   automatically in the form \code{"p0-p10"}, \code{"p10-p20"}, etc.
#' @param varname Optional name for the new grouping variable. Defaults to
#'   \code{"{var}_pctile"} (e.g., \code{"networth_pctile"}).
#' @param method Character. One of \code{"implicate"} (default) or
#'   \code{"stack"}. See Details.
#' @param stat Character. One of \code{"mean"} (default), \code{"median"},
#'   or \code{"none"}. When \code{"none"}, returns the updated
#'   \code{scf_mi_survey} object with the grouping variable added and no
#'   summary statistic computed. When \code{"mean"} or \code{"median"},
#'   computes the statistic within each percentile group and returns a data
#'   frame of results.
#' @param stat_var Optional one-sided formula naming the variable to summarize
#'   within each group. Defaults to \code{var} if not supplied.
#'
#' @return When \code{stat = "none"}, returns the input \code{scf_mi_survey}
#'   object with a new factor variable added to each implicate's data frame.
#'   The variable is named according to \code{varname} (default:
#'   \code{"{var}_pctile"}), and can be passed directly to
#'   \code{\link{scf_mean}}, \code{\link{scf_median}}, or other estimation
#'   functions via the \code{by} argument. When \code{stat = "mean"} or
#'   \code{"median"} with \code{method = "implicate"}, returns the output of
#'   \code{\link{scf_mean}} or \code{\link{scf_median}} respectively. When
#'   \code{stat != "none"} with \code{method = "stack"}, returns a data frame
#'   with columns \code{group}, \code{variable}, and \code{estimate}. No
#'   standard error is returned for the stack method.
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("pctile_sum_")
#' dir.create(td)
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Mean net worth, top vs bottom 90 percent, stack method (fast)
#' scf_pctile_sum(scf2022, ~networth,
#'                probs  = c(0, 0.9, 1),
#'                labels = c("bottom90", "top10"),
#'                method = "stack")
#'
#' \dontrun{
#' # Implicate method (default): requires full SCF data; unreliable on mock data
#' scf_pctile_sum(scf2022, ~networth)
#'
#' # Return grouping variable only, no summary statistic (implicate method)
#' scf2022 <- scf_pctile_sum(scf2022, ~networth,
#'                            probs  = c(0, 0.9, 1),
#'                            labels = c("bottom90", "top10"),
#'                            stat   = "none")
#' }
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @seealso \code{\link{scf_mean}}, \code{\link{scf_median}},
#'   \code{\link{scf_percentile}}, \code{\link{scf_update_by_implicate}}
#'
#' @export
scf_pctile_sum <- function(scf, var,
                           probs    = seq(0, 1, by = 0.1),
                           labels   = NULL,
                           varname  = NULL,
                           method   = c("implicate", "stack"),
                           stat     = c("mean", "median", "none"),
                           stat_var = NULL) {
  
  method <- match.arg(method)
  stat   <- match.arg(stat)
  
  # --- Input validation ---
  if (!inherits(scf, "scf_mi_survey")) {
    stop("`scf` must be an object of class 'scf_mi_survey'.", call. = FALSE)
  }
  if (!inherits(var, "formula")) {
    stop("`var` must be a one-sided formula (e.g., ~networth).", call. = FALSE)
  }
  if (!is.numeric(probs) || any(probs < 0) || any(probs > 1)) {
    stop("`probs` must be a numeric vector with values in [0, 1].", call. = FALSE)
  }
  if (!is.null(stat_var) && !inherits(stat_var, "formula")) {
    stop("`stat_var` must be a one-sided formula (e.g., ~networth).", call. = FALSE)
  }
  
  probs <- sort(unique(probs))
  
  if (probs[1] != 0 || probs[length(probs)] != 1) {
    stop("`probs` must include 0 and 1 as endpoints.", call. = FALSE)
  }
  
  n_groups <- length(probs) - 1
  
  if (n_groups < 2) {
    stop("`probs` must define at least two groups (length >= 3).", call. = FALSE)
  }
  
  if (!is.null(labels)) {
    if (!is.character(labels) || length(labels) != n_groups) {
      stop(sprintf("`labels` must be a character vector of length %d.", n_groups),
           call. = FALSE)
    }
  }
  
  if (isTRUE(attr(scf, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.",
            call. = FALSE)
  }
  
  # --- Derived values ---
  varname_in <- all.vars(var)[1]
  
  if (is.null(varname)) {
    varname <- paste0(varname_in, "_pctile")
  }
  
  if (is.null(stat_var)) {
    stat_var <- var
  }
  
  stat_varname <- all.vars(stat_var)[1]
  interior_probs <- probs[probs > 0 & probs < 1]
  
  if (is.null(labels)) {
    labels <- paste0("p", round(probs[-length(probs)] * 100),
                     "-p", round(probs[-1] * 100))
  }
  
  # --- Stack method (Federal Reserve convention) ---
  if (method == "stack") {
    M <- length(scf$mi_design)
    
    all_vars <- do.call(rbind, lapply(seq_along(scf$mi_design), function(i) {
      df <- scf$mi_design[[i]]$variables
      df$.implicate <- i
      df$.row_id <- seq_len(nrow(df))
      df
    }))
    
    all_vars$.wgt_stack <- all_vars$wgt / M
    
    # Identify household/person ID for SAS-style tie ordering.
    # The Federal Reserve SAS macro sorts by the percentile variable and PID.
    id_candidates <- c("y1", "Y1", "x1", "X1")
    id_var <- id_candidates[id_candidates %in% names(all_vars)][1]
    
    if (is.na(id_var)) {
      stop("Could not find household ID variable (`y1`, `Y1`, `x1`, or `X1`) ",
           "needed for stack-method percentile assignment.",
           call. = FALSE)
    }
    
    keep <- is.finite(all_vars[[varname_in]]) &
      is.finite(all_vars$.wgt_stack) &
      all_vars$.wgt_stack > 0
    
    if (stat != "none") {
      keep <- keep & is.finite(all_vars[[stat_varname]])
    }
    
    all_vars <- all_vars[keep, , drop = FALSE]
    
    # Federal Reserve SAS macro logic:
    # sort by percentile variable and household/person ID, then assign
    # percentile groups from cumulative weighted population share.
    ord <- order(all_vars[[varname_in]], all_vars[[id_var]])
    all_vars <- all_vars[ord, , drop = FALSE]
    
    cumshare <- cumsum(all_vars$.wgt_stack) / sum(all_vars$.wgt_stack)
    
    group_num <- rep(1L, length(cumshare))
    
    for (j in seq_len(n_groups)) {
      group_num[cumshare >= probs[j]] <- j
    }
    
    group_num[group_num < 1L] <- 1L
    group_num[group_num > n_groups] <- n_groups
    
    all_vars[[varname]] <- factor(labels[group_num], levels = labels)
    
    # If no summary statistic requested, add grouping variable to each
    # implicate and return the scf_mi_survey object. This uses the same
    # stack-level category assignment, then maps the assigned group back
    # to each original implicate row.
    if (stat == "none") {
      split_vars <- split(all_vars, all_vars$.implicate)
      
      updated_designs <- vector("list", M)
      
      for (i in seq_along(scf$mi_design)) {
        df_orig <- scf$mi_design[[i]]$variables
        df_new  <- split_vars[[as.character(i)]]
        
        if (is.null(df_new)) {
          stop("Stack-method grouping failed for implicate ", i, ".", call. = FALSE)
        }
        
        df_orig[[varname]] <- NA_character_
        df_orig[[varname]][df_new$.row_id] <- as.character(df_new[[varname]])
        df_orig[[varname]] <- factor(df_orig[[varname]], levels = labels)
        
        rep_cols <- grep("^wt1b", names(df_orig), value = TRUE)
        if (length(rep_cols) == 0) {
          stop("Could not find replicate weight columns in implicate.", call. = FALSE)
        }
        
        updated_designs[[i]] <- survey::svrepdesign(
          weights          = ~wgt,
          repweights       = as.matrix(df_orig[, rep_cols]),
          data             = df_orig,
          type             = "other",
          scale            = 1,
          rscales          = rep(1 / (length(rep_cols) - 1), length(rep_cols)),
          mse              = TRUE,
          combined.weights = TRUE
        )
      }
      
      scf$mi_design <- updated_designs
      return(scf)
    }
    
    # Flat weighted statistic on stacked data, Fed convention.
    out <- do.call(rbind, lapply(labels, function(g) {
      idx <- all_vars[[varname]] == g
      
      est <- if (stat == "mean") {
        weighted.mean(all_vars[[stat_varname]][idx],
                      all_vars$.wgt_stack[idx],
                      na.rm = TRUE)
      } else {
        .scf_wtd_median(x = all_vars[[stat_varname]][idx],
                        w = all_vars$.wgt_stack[idx])
      }
      
      data.frame(group    = g,
                 variable = stat_varname,
                 estimate = est,
                 stringsAsFactors = FALSE)
    }))
    
    rownames(out) <- NULL
    return(out)
  }
  
  # --- Implicate method (per-implicate survey-weighted quantiles) ---
  updated_designs <- vector("list", length(scf$mi_design))
  
  for (i in seq_along(scf$mi_design)) {
    design <- scf$mi_design[[i]]
    df     <- design$variables
    
    thresholds <- as.numeric(coef(
      suppressWarnings(suppressMessages(
        survey::svyquantile(var, design,
                            quantiles     = interior_probs,
                            se            = TRUE,
                            interval.type = "quantile")
      ))
    ))
    
    breaks <- c(-Inf, thresholds, Inf)
    
    df[[varname]] <- cut(df[[varname_in]],
                         breaks         = breaks,
                         labels         = labels,
                         include.lowest = TRUE,
                         right          = FALSE)
    
    rep_cols <- grep("^wt1b", names(df), value = TRUE)
    if (length(rep_cols) == 0) {
      stop("Could not find replicate weight columns in implicate.", call. = FALSE)
    }
    
    updated_designs[[i]] <- survey::svrepdesign(
      weights          = ~wgt,
      repweights       = as.matrix(df[, rep_cols]),
      data             = df,
      type             = "other",
      scale            = 1,
      rscales          = rep(1 / (length(rep_cols) - 1), length(rep_cols)),
      mse              = TRUE,
      combined.weights = TRUE
    )
  }
  
  scf$mi_design <- updated_designs
  
  # Return updated object if no stat requested
  if (stat == "none") return(scf)
  
  # Compute summary statistic using Rubin's rules
  by_formula <- stats::as.formula(paste0("~", varname))
  
  if (stat == "mean") {
    return(scf_mean(scf, stat_var, by = by_formula))
  } else {
    return(scf_median(scf, stat_var, by = by_formula))
  }
}


#' @rdname scf_pctile_sum
#' @export
scf_pctile_cut <- scf_pctile_sum


# Internal helper: weighted median for stacked-data summaries.
# This is used only when stat = "median"; percentile-group construction
# under method = "stack" is handled directly by cumulative weighted
# population-share assignment in scf_pctile_sum().
.scf_wtd_median <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]
  w <- w[ok]
  
  if (length(x) == 0L) return(NA_real_)
  
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  
  cum_w <- cumsum(w) / sum(w)
  x[which(cum_w >= 0.5)[1L]]
}