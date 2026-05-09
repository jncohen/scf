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
#' in which all five implicates are pooled with weights divided by five, a
#' single set of thresholds is computed from the pooled distribution, and a
#' flat weighted statistic is computed directly on the stacked data.
#'
#' @details
#' The two methods will generally produce similar but not identical results.
#' Use \code{method = "stack"} when exact replication of the Federal Reserve's
#' published SCF tables is required. Note that \code{method = "stack"} with
#' \code{stat != "none"} computes a flat weighted statistic on the pooled
#' stacked data and does not use replicate-weight survey machinery or Rubin's
#' combining rules. No standard error is returned in this case.
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
  varname_in     <- all.vars(var)[1]
  if (is.null(varname))  varname  <- paste0(varname_in, "_pctile")
  if (is.null(stat_var)) stat_var <- var
  stat_varname   <- all.vars(stat_var)[1]
  interior_probs <- probs[probs > 0 & probs < 1]
  
  if (is.null(labels)) {
    labels <- paste0("p", round(probs[-length(probs)] * 100),
                     "-p", round(probs[-1] * 100))
  }
  
  # --- Stack method (Federal Reserve convention) ---
  if (method == "stack") {
    M        <- length(scf$mi_design)
    all_vars <- do.call(rbind, lapply(scf$mi_design, function(d) d$variables))
    w        <- all_vars$wgt / M
    
    thresholds <- .scf_wtd_quantile(x     = all_vars[[varname_in]],
                                    w     = w,
                                    probs = interior_probs)
    breaks <- c(-Inf, thresholds, Inf)
    
    all_vars[[varname]] <- cut(all_vars[[varname_in]],
                               breaks         = breaks,
                               labels         = labels,
                               include.lowest = TRUE,
                               right          = FALSE)
    
    # If no summary statistic requested, add grouping variable to each
    # implicate and return the scf_mi_survey object
    if (stat == "none") {
      return(scf_update_by_implicate(scf, function(df) {
        df[[varname]] <- cut(df[[varname_in]],
                             breaks         = breaks,
                             labels         = labels,
                             include.lowest = TRUE,
                             right          = FALSE)
        df
      }))
    }
    
    # Flat weighted statistic on stacked data, Fed convention
    grp_levels <- levels(all_vars[[varname]])
    out <- do.call(rbind, lapply(grp_levels, function(g) {
      idx <- all_vars[[varname]] == g
      est <- if (stat == "mean") {
        weighted.mean(all_vars[[stat_varname]][idx], w[idx], na.rm = TRUE)
      } else {
        # weighted median via .scf_wtd_quantile
        .scf_wtd_quantile(x     = all_vars[[stat_varname]][idx],
                          w     = w[idx],
                          probs = 0.5)
      }
      data.frame(group    = g,
                 variable = stat_varname,
                 estimate = est,
                 stringsAsFactors = FALSE)
    }))
    
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


# Internal helper: weighted quantile matching the Federal Reserve's stacking
# convention. Finds the first observation at which the cumulative weighted
# population share reaches or exceeds each requested probability.
.scf_wtd_quantile <- function(x, w, probs) {
  ord   <- order(x)
  x     <- x[ord]
  w     <- w[ord]
  cum_w <- cumsum(w) / sum(w)
  sapply(probs, function(p) x[which(cum_w >= p)[1L]])
}