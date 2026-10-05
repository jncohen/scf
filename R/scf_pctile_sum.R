#' Summarize SCF Variables by Percentile Group
#'
#' Groups households by percentiles of a variable and summarizes a variable
#' within each group.
#'
#' @details
#' Use it to compare households at different points of a distribution, such
#' as mean net worth of the top 10 percent against the bottom 50 percent.
#' Each row is one group; the estimate is the mean or median of `stat_var` in
#' that group. For the Fed's own groups, such as `nwcat` and `inccat`, use
#' `by` in [scf_mean()] or [scf_median()] instead.
#'
#' Households are sorted by the variable and household identifier, and the
#' cumulative weighted population share sets each household's group, as in the
#' Fed's SAS macro. Ties at a boundary are split by identifier, so each group
#' holds its share of the population.
#'
#' `method = "implicate"` assigns groups within each implicate and returns
#' estimates with standard errors. `method = "stack"` stacks the implicates
#' with weights divided by five, assigns groups once, and computes a weighted
#' statistic without a standard error; it reproduces the Fed's published
#' figures. With `stat = "none"`, the data are returned with the group
#' variable added.
#'
#' @param scf A `scf_mi_survey` object created with [scf_load()].
#' @param var A one-sided formula naming the continuous variable to cut
#'   (e.g., `~networth`).
#' @param probs Numeric vector of values between 0 and 1 defining group boundaries,
#'   including 0 and 1 as endpoints. Defaults to deciles
#'   (`seq(0, 1, by = 0.1)`).
#' @param labels Optional character vector of group labels, length equal to
#'   `length(probs) - 1`. If `NULL` (default), labels are generated
#'   automatically in the form `"p0-p10"`, `"p10-p20"`, etc.
#' @param varname Optional name for the new grouping variable. Defaults to
#'   \code{"{var}_pctile"} (e.g., `"networth_pctile"`).
#' @param method Character. One of `"implicate"` (default) or
#'   `"stack"`. See Details.
#' @param stat `"mean"` (default), `"median"`, or `"none"`. With `"none"`,
#'   no statistic is computed and the data are returned with the group
#'   variable added.
#' @param stat_var Optional one-sided formula naming the variable to summarize
#'   within each group. Defaults to `var` if not supplied.
#'
#' @return With `stat = "none"`, the `scf_mi_survey` object with the group
#'   variable (named by `varname`) added; pass it to `by` in other functions.
#'   With `method = "implicate"`, the result of [scf_mean()] or
#'   [scf_median()]. With `method = "stack"`, a data frame with columns
#'   `group`, `variable`, and `estimate`.
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
#' @seealso [scf_mean()], [scf_median()],
#'   [scf_percentile()], [scf_update_by_implicate()]
#'
#' @references
#' Board of Governors of the Federal Reserve System. SAS macro for SCF
#'   Bulletin variable definitions (bulletin.macro.txt).
#'   <https://www.federalreserve.gov/econres/files/bulletin.macro.txt>
#'
#' @export
scf_pctile_sum <- function(scf, var,
                           probs = seq(0, 1, by = 0.1),
                           labels = NULL,
                           varname = NULL,
                           method = c("implicate", "stack"),
                           stat = c("mean", "median", "none"),
                           stat_var = NULL) {

  method <- match.arg(method)
  stat <- match.arg(stat)

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

  varname_in <- all.vars(var)[1]

  if (is.null(varname)) {
    varname <- paste0(varname_in, "_pctile")
  }

  if (is.null(stat_var)) {
    stat_var <- var
  }

  stat_varname <- all.vars(stat_var)[1]
  .scf_check_na(scf$mi_design, var, stat_var)

  if (is.null(labels)) {
    labels <- paste0("p", round(probs[-length(probs)] * 100),
                     "-p", round(probs[-1] * 100))
  }

  id_candidates <- c("y1", "Y1", "x1", "X1")
  id_var <- id_candidates[id_candidates %in% names(scf$mi_design[[1]]$variables)][1]

  if (method == "stack") {
    M <- length(scf$mi_design)

    all_vars <- do.call(rbind, lapply(seq_along(scf$mi_design), function(i) {
      df <- scf$mi_design[[i]]$variables
      df$.implicate <- i
      df$.row_id <- seq_len(nrow(df))
      df
    }))

    all_vars$.wgt_stack <- all_vars$wgt / M

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

    group_num <- .scf_pctile_groups(all_vars[[varname_in]], all_vars$.wgt_stack,
                                    all_vars[[id_var]], probs)

    all_vars[[varname]] <- factor(labels[group_num], levels = labels)

    if (stat == "none") {
      split_vars <- split(all_vars, all_vars$.implicate)

      updated_designs <- vector("list", M)

      for (i in seq_along(scf$mi_design)) {
        df_orig <- scf$mi_design[[i]]$variables
        df_new <- split_vars[[as.character(i)]]

        if (is.null(df_new)) {
          stop("Stack-method grouping failed for implicate ", i, ".", call. = FALSE)
        }

        df_orig[[varname]] <- NA_character_
        df_orig[[varname]][df_new$.row_id] <- as.character(df_new[[varname]])
        df_orig[[varname]] <- factor(df_orig[[varname]], levels = labels)

        updated_designs[[i]] <- .scf_svrepdesign(df_orig)
      }

      scf$mi_design <- updated_designs
      return(scf)
    }

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

      data.frame(group = g,
                 variable = stat_varname,
                 estimate = est,
                 stringsAsFactors = FALSE)
    }))

    rownames(out) <- NULL
    return(out)
  }

  updated_designs <- vector("list", length(scf$mi_design))

  for (i in seq_along(scf$mi_design)) {
    design <- scf$mi_design[[i]]
    df <- design$variables
    id <- if (is.na(id_var)) seq_len(nrow(df)) else df[[id_var]]

    group_num <- .scf_pctile_groups(df[[varname_in]],
                                    as.numeric(stats::weights(design, "sampling")),
                                    id, probs)
    df[[varname]] <- factor(labels[group_num], levels = labels)

    updated_designs[[i]] <- .scf_svrepdesign(df)
  }

  scf$mi_design <- updated_designs

  if (stat == "none") return(scf)

  by_formula <- stats::as.formula(paste0("~", varname))

  if (stat == "mean") {
    return(scf_mean(scf, stat_var, by = by_formula))
  } else {
    return(scf_median(scf, stat_var, by = by_formula))
  }
}

#' Deprecated: use scf_pctile_sum()
#'
#' `scf_pctile_cut()` was renamed [scf_pctile_sum()] in version 1.0.7 and will
#' be removed in a future release.
#'
#' @param ... Arguments passed to [scf_pctile_sum()].
#' @return The result of [scf_pctile_sum()].
#' @keywords internal
#' @export
scf_pctile_cut <- function(...) {
  .Deprecated("scf_pctile_sum")
  scf_pctile_sum(...)
}

.scf_pctile_groups <- function(x, w, id, probs) {
  ord <- order(x, id)
  cumshare <- numeric(length(x))
  cumshare[ord] <- cumsum(w[ord]) / sum(w)
  grp <- rep(1L, length(x))
  for (j in seq_len(length(probs) - 1)) {
    grp[cumshare >= probs[j]] <- j
  }
  grp
}
