#' Estimate Percentiles in SCF Microdata
#'
#' Estimates a weighted percentile of a continuous variable, overall or by
#' group.
#'
#' @details
#' Use it for points of a distribution other than the middle, such as the
#' 90th percentile of net worth. With `q = 0.9`, 90 percent of households are
#' below the estimate. Dollar amounts are in 2022 dollars.
#'
#' `method = "implicate"` estimates the percentile in each implicate with
#' [survey::svyquantile()] and pools the estimates (see [scf_variance]).
#' `method = "stack"` stacks the implicates with weights divided by five and
#' computes one weighted percentile without a standard error; it reproduces
#' the Fed's published figures.
#'
#' Warnings from [survey::svyquantile()] are passed on with the implicate
#' number. One notice, "Not all replicate weight designs give valid standard
#' errors for quantiles", is dropped; it concerns jackknife weights, and the
#' SCF's are bootstrap weights.
#'
#' @param scf A `scf_mi_survey` object created with [scf_load()]. Must
#'   contain the list of replicate-weighted designs for each implicate in
#'   `scf$mi_design`.
#' @param var A one-sided formula naming the continuous variable to
#'   summarize (for example `~networth`).
#' @param q Percentile, between 0 and 1. Default 0.5 (median).
#' @param method `"implicate"` (default) or `"stack"`. See Details.
#' @param by Optional one-sided formula naming a categorical grouping
#'   variable. If supplied, the percentile is estimated separately within
#'   each group.
#' @param verbose Logical. If TRUE, print implicate-level estimates.
#'   Default FALSE.
#' @param variance Variance method: `"fed"` (default) or `"rubin"`. See [scf_variance].
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
#' td <- tempfile("percentile_")
#' dir.create(td)
#'
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
#' unlink(td, recursive = TRUE, force = TRUE)
#' rm(scf2022)
#'
#' @references
#' Board of Governors of the Federal Reserve System. SAS macro for SCF
#'   Bulletin variable definitions (bulletin.macro.txt).
#'   <https://www.federalreserve.gov/econres/files/bulletin.macro.txt>
#'
#' @seealso [scf_median()], [scf_mean()]
#'
#' @export
scf_percentile <- function(scf, var, q = 0.5, by = NULL,
                           verbose = FALSE,
                           method = c("implicate", "stack"),
                           variance = getOption("scf.variance", "fed")) {

  method <- match.arg(method)
  variance <- .scf_variance_method(variance)

  if (!inherits(scf, "scf_mi_survey")) {
    stop("`scf` must be an object of class 'scf_mi_survey'.", call. = FALSE)
  }
  if (!is.numeric(q) || length(q) != 1L || q < 0 || q > 1) {
    stop("`q` must be a single numeric value in [0, 1].", call. = FALSE)
  }

  varname <- all.vars(var)[1]
  byname <- if (!is.null(by)) all.vars(by)[1] else NULL
  .scf_check_na(scf$mi_design, var, by)

  designs <- scf$mi_design
  M <- length(designs)

  if (!is.null(byname)) {
    groups <- .scf_group_levels(designs, byname)
    for (i in seq_len(M)) {
      designs[[i]]$variables[[byname]] <- factor(designs[[i]]$variables[[byname]],
                                                 levels = groups)
    }
  }

  get_quantile_obj <- function(dsgn, vname, qval, i, g = NULL, gname = NULL) {
    if (!is.null(g)) {
      keep <- !is.na(dsgn$variables[[gname]]) & dsgn$variables[[gname]] == g
      dsgn <- dsgn[keep, ]
    }

    form <- stats::as.formula(paste0("~", vname))

    withCallingHandlers(
      survey::svyquantile(
        form,
        dsgn,
        quantiles = qval,
        se = TRUE,
        interval.type = "quantile"
      ),
      message = function(m) {
        if (!grepl("Not all replicate weight designs give valid standard errors",
                   conditionMessage(m), fixed = TRUE)) {
          message(sprintf("Implicate %d: %s", i, conditionMessage(m)))
        }
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        warning(sprintf("Implicate %d: %s", i, conditionMessage(w)), call. = FALSE)
        invokeRestart("muffleWarning")
      }
    )
  }

  if (method == "stack") {
    all_vars <- do.call(rbind, lapply(designs, function(d) d$variables))
    if (!is.null(byname)) {
      groups <- levels(factor(all_vars[[byname]]))
      out_df <- do.call(rbind, lapply(groups, function(g) {
        idx <- all_vars[[byname]] == g
        est <- .scf_wtd_quantile(x = all_vars[[varname]][idx],
                                 w = all_vars$wgt[idx] / M,
                                 probs = q)
        data.frame(group = g, variable = varname, quantile = q,
                   estimate = est, se = NA_real_,
                   min = NA_real_, max = NA_real_,
                   stringsAsFactors = FALSE)
      }))
    } else {
      est <- .scf_wtd_quantile(x = all_vars[[varname]],
                               w = all_vars$wgt / M,
                               probs = q)
      out_df <- data.frame(variable = varname, quantile = q,
                           estimate = est, se = NA_real_,
                           min = NA_real_, max = NA_real_,
                           stringsAsFactors = FALSE)
    }
    return(structure(
      list(results = out_df, imps = NULL,
           aux = list(varname = varname, byname = byname, quantile = q,
                          year = scf$year),
           verbose = verbose),
      class = "scf_percentile"
    ))
  }

  if (is.null(byname)) {

    imp_objs <- lapply(seq_len(M), function(i) {
      get_quantile_obj(designs[[i]], varname, q, i)
    })

    imp_estimates <- lapply(seq_len(M), function(i) {
      obj_i <- imp_objs[[i]]
      data.frame(
        implicate = i,
        group = "All",
        quantile = q,
        estimate = as.numeric(stats::coef(obj_i)),
        se = sqrt(diag(stats::vcov(obj_i))),
        stringsAsFactors = FALSE
      )
    })

    point_vec <- sapply(imp_objs, function(o) as.numeric(stats::coef(o)))
    qbar <- mean(point_vec)

    var_vec <- sapply(imp_objs, function(o) as.numeric(stats::vcov(o)))
    se_pooled <- .scf_pool(point_vec, var_vec, variance)$se

    out_df <- data.frame(
      variable = varname,
      quantile = q,
      estimate = qbar,
      se = se_pooled,
      min = min(point_vec),
      max = max(point_vec),
      stringsAsFactors = FALSE
    )

  } else {

    imp_objs <- lapply(seq_len(M), function(i) {
      dsgn_i <- designs[[i]]
      lapply(groups, function(g) {
        if (!any(dsgn_i$variables[[byname]] == g, na.rm = TRUE)) return(NULL)
        get_quantile_obj(dsgn_i, varname, q, i, g = g, gname = byname)
      })
    })

    imp_estimates <- lapply(seq_len(M), function(i) {
      objs_i <- imp_objs[[i]]
      do.call(rbind, lapply(seq_along(groups), function(j) {
        obj_ij <- objs_i[[j]]
        if (is.null(obj_ij)) return(NULL)
        data.frame(
          implicate = i,
          group = groups[j],
          quantile = q,
          estimate = as.numeric(stats::coef(obj_ij)),
          se = sqrt(diag(stats::vcov(obj_ij))),
          stringsAsFactors = FALSE
        )
      }))
    })

    out_df <- do.call(rbind, lapply(seq_along(groups), function(j) {
      group_objs <- lapply(seq_len(M), function(i) imp_objs[[i]][[j]])
      point_vec <- sapply(group_objs, function(o) {
        if (is.null(o)) NA_real_ else as.numeric(stats::coef(o))
      })
      var_vec <- sapply(group_objs, function(o) {
        if (is.null(o)) NA_real_ else as.numeric(stats::vcov(o))
      })
      pooled_g <- .scf_pool(point_vec, var_vec, variance)
      qbar_g <- pooled_g$estimate
      se_pooled_g <- pooled_g$se

      data.frame(
        group = groups[j],
        variable = varname,
        quantile = q,
        estimate = qbar_g,
        se = se_pooled_g,
        min = min(point_vec, na.rm = TRUE),
        max = max(point_vec, na.rm = TRUE),
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
      imps = imp_estimates,
      aux = list(
        varname = varname,
        byname = byname,
        quantile = q,
        year = scf$year
      ),
      verbose = verbose
    ),
    class = "scf_percentile"
  )
}

#' @export
print.scf_percentile <- function(x, ...) {
  dollar_label <- if (isTRUE(attr(x, "nominal")))
    sprintf(" (nominal %d dollars)", x$aux$year) else ""
  cat(sprintf("SCF Percentile Estimate%s\n\n", dollar_label))

  print(x$results, row.names = FALSE, ...)
  if (all(is.na(x$results$se))) {
    cat("\nNote: Standard error not available for stack method.\n")
  }
  if (isTRUE(x$verbose) && !is.null(x$imps)) {
    cat("\nImplicate-Level Estimates:\n\n")
    imp_df <- do.call(rbind, x$imps)
    print(imp_df, row.names = FALSE)
  }
  invisible(x)
}

#' @export
summary.scf_percentile <- function(object, ...) {
  cat("Summary of SCF Percentile Estimate\n\n")
  cat("Pooled Estimates:\n")
  print(format(object$results, digits = 4, nsmall = 2),
        row.names = FALSE, ...)
  if (!is.null(object$imps)) {
    cat("\nImplicate-Level Estimates:\n")
    imp_df <- do.call(rbind, object$imps)
    print(format(imp_df, digits = 4, nsmall = 2), row.names = FALSE)
  } else {
    cat("\nImplicate-level estimates not available for stack method.\n")
  }
  invisible(object)
}
