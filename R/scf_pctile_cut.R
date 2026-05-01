#' Create Percentile-Based Grouping Variables in SCF Data
#'
#' @description
#' Creates a factor variable that assigns each household to a percentile group
#' of a continuous variable. Two methods are available. The \code{"implicate"}
#' method (the default) computes survey-weighted quantile thresholds separately
#' within each implicate, the statistically preferable approach under multiple
#' imputation, as it correctly accounts for between-implicate variation in
#' imputed values. The \code{"stack"} method replicates the Federal Reserve's
#' published convention, in which all five implicates are pooled with weights
#' divided by five, a single set of thresholds is computed from the pooled
#' distribution, and those fixed thresholds are applied uniformly across
#' implicates.
#'
#' @details
#' The two methods will generally produce similar but not identical results.
#' Use \code{method = "stack"} when exact replication of the Federal Reserve's
#' published SCF tables is required. For standard grouping variables already
#' published by the Fed, such as net worth percentile (\code{nwcat}) and
#' income percentile (\code{inccat}) - those variables are included directly
#' in the data returned by \code{\link{scf_load}} and can be passed to
#' \code{by} in estimation functions without calling \code{scf_pctile_cut}.
#'
#' @param scf A \code{scf_mi_survey} object created with \code{\link{scf_load}}.
#' @param var A one-sided formula naming the continuous variable to cut
#'   (e.g., \code{~networth}).
#' @param probs numeric vector with values between 0 and 1
#' @param labels Optional character vector of group labels, length equal to
#'   \code{length(probs) - 1}. If \code{NULL} (default), labels are generated
#'   automatically in the form \code{"p0-p10"}, \code{"p10-p20"}, etc.
#' @param varname Optional name for the new grouping variable. Defaults to
#'   \code{"{var}_pctile"} (e.g., \code{"networth_pctile"}).
#' @param method Character. One of \code{"implicate"} (default) or
#'   \code{"stack"}. See Details.
#'
#' @return The input \code{scf_mi_survey} object with the new factor variable
#'   added to each implicate's data frame. Pass directly to
#'   \code{\link{scf_mean}}, \code{\link{scf_median}}, or other estimation
#'   functions via the \code{by} argument.
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("pctile_cut_")
#' dir.create(td)
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Top vs bottom median using stack method
#' scf2022 <- scf_pctile_cut(scf2022, ~networth,
#'                           probs  = c(0, 0.5, 1),
#'                           labels = c("bottom50", "top50"),
#'                           method = "stack")
#'                           
#' levels(scf2022$mi_design[[1]]$variables$networth_pctile)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @seealso \code{\link{scf_update_by_implicate}}, \code{\link{scf_mean}},
#'   \code{\link{scf_percentile}}
#'
#' @export
scf_pctile_cut <- function(scf, var, probs = seq(0, 1, by = 0.1),
                            labels = NULL, varname = NULL,
                            method = c("implicate", "stack")) {

  method <- match.arg(method)

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
  if (is.null(varname)) varname <- paste0(varname_in, "_pctile")
  interior_probs <- probs[probs > 0 & probs < 1]

  if (is.null(labels)) {
    labels <- paste0("p", round(probs[-length(probs)] * 100),
                     "-p", round(probs[-1] * 100))
  }

  # --- Stack method (Federal Reserve convention) ---
  if (method == "stack") {
    M        <- length(scf$mi_design)
    all_vars <- do.call(rbind, lapply(scf$mi_design, function(d) d$variables))
    thresholds <- .scf_wtd_quantile(x     = all_vars[[varname_in]],
                                    w     = all_vars$wgt / M,
                                    probs = interior_probs)
    breaks <- c(-Inf, thresholds, Inf)

    return(scf_update_by_implicate(scf, function(df) {
      df[[varname]] <- cut(df[[varname_in]],
                           breaks         = breaks,
                           labels         = labels,
                           include.lowest = TRUE,
                           right          = FALSE)
      df
    }))
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
  scf
  
}


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
