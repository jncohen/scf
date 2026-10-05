#' Estimate Mean in Multiply-Imputed SCF Data
#'
#' Estimates the weighted mean of a continuous variable, overall or by group.
#'
#' @details
#' Use it for the average value of a variable in the population or a group.
#' The estimate is in the variable's units; dollar amounts are in 2022
#' dollars. `min` and `max` give the range of the implicate estimates.
#' Wealth and income are right-skewed, so a few very wealthy households pull
#' their means well above their medians; report [scf_median()] as well.
#'
#' The mean is estimated in each implicate with `survey::svymean()` and pooled
#' across implicates (see [scf_variance]).
#'
#' @param scf A scf_mi_survey object created with [scf_load()]. Must contain five replicate-weighted implicates.
#' @param var A one-sided formula identifying the continuous variable to summarize (e.g., ~networth).
#' @param by Optional one-sided formula specifying a discrete grouping variable for stratified means.
#' @param verbose Logical. If TRUE, include implicate-level results in print output. Default is FALSE.
#' @param variance Variance method: `"fed"` (default) or `"rubin"`. See [scf_variance].
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
#' td <- tempfile("mean_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Estimate means
#' scf_mean(scf2022, ~networth)
#' scf_mean(scf2022, ~networth, by = ~edcl)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#'
#' @seealso [scf_median()], [scf_percentile()], [scf_xtab()], [scf_plot_dist()]
#'
#' @export
scf_mean <- function(scf, var, by = NULL, verbose = FALSE,
                     variance = getOption("scf.variance", "fed")) {
  variance <- .scf_variance_method(variance)

  if (!inherits(scf, "scf_mi_survey") ||
      !is.list(scf$mi_design) ||
      !all(sapply(scf$mi_design, inherits, "svyrep.design"))) {
    stop("Input must be a 'scf_mi_survey' object with replicate-weighted implicates.")
  }

  varname <- all.vars(var)[1]
  byname <- if (!is.null(by)) all.vars(by)[1] else NULL
  .scf_check_na(scf$mi_design, var, by)
  if (!is.null(by) && length(all.vars(var)) > 1) {
    stop("With `by`, `var` must name one variable.", call. = FALSE)
  }
  designs <- scf$mi_design
  nimp <- length(designs)

  if (is.null(byname)) {

    imp_results <- lapply(seq_len(nimp), function(i) {
      d <- designs[[i]]
      survey::svymean(var, d)
    })

    imp_estimates <- lapply(seq_len(nimp), function(i) {
      est <- imp_results[[i]]
      data.frame(
        implicate = i,
        group = "All",
        variable = names(coef(est)),
        estimate = as.numeric(coef(est)),
        se = sqrt(diag(vcov(est))),
        stringsAsFactors = FALSE
      )
    })

    x_all <- do.call(rbind, lapply(imp_results, function(o) as.numeric(coef(o))))
    v_all <- do.call(rbind, lapply(imp_results, function(o) diag(as.matrix(vcov(o)))))
    pooled <- .scf_pool(x_all, v_all, variance)

    out <- data.frame(
      variable = names(coef(imp_results[[1]])),
      estimate = pooled$estimate,
      se = pooled$se,
      min = apply(x_all, 2, min),
      max = apply(x_all, 2, max),
      stringsAsFactors = FALSE
    )

  } else {

    groups <- .scf_group_levels(designs, byname)
    for (i in seq_len(nimp)) {
      designs[[i]]$variables[[byname]] <- factor(designs[[i]]$variables[[byname]],
                                                 levels = groups)
    }

    imp_results <- lapply(seq_len(nimp), function(i) {
      d <- designs[[i]]
      lapply(groups, function(g) {
        keep <- !is.na(d$variables[[byname]]) & d$variables[[byname]] == g
        if (!any(keep)) return(NULL)
        survey::svymean(var, d[keep, ])
      })
    })

    imp_estimates <- lapply(seq_len(nimp), function(i) {
      res_i <- imp_results[[i]]
      do.call(rbind, lapply(seq_along(groups), function(j) {
        est <- res_i[[j]]
        if (is.null(est)) return(NULL)
        data.frame(
          implicate = i,
          group = groups[j],
          estimate = as.numeric(coef(est)),
          se = sqrt(diag(vcov(est))),
          stringsAsFactors = FALSE
        )
      }))
    })

    out <- do.call(rbind, lapply(seq_along(groups), function(j) {
      g <- groups[j]

      res_list_g <- lapply(seq_len(nimp), function(i) {
        imp_results[[i]][[j]]
      })

      x_g <- sapply(res_list_g, function(o) if (is.null(o)) NA_real_ else as.numeric(coef(o)))
      v_g <- sapply(res_list_g, function(o) if (is.null(o)) NA_real_ else as.numeric(vcov(o)))
      pooled_g <- .scf_pool(x_g, v_g, variance)
      est_pooled_g <- pooled_g$estimate
      se_pooled_g <- pooled_g$se

      data.frame(
        group = g,
        variable = varname,
        estimate = est_pooled_g,
        se = se_pooled_g,
        min = min(x_g, na.rm = TRUE),
        max = max(x_g, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))

    out <- out[, c("group", "variable", "estimate", "se", "min", "max")]
  }

  names(imp_estimates) <- paste0("imp", seq_len(nimp))

  out_obj <- list(
    results = out,
    imps = imp_estimates,
    aux = list(varname = varname, byname = byname, year = scf$year),
    verbose = verbose
  )
  class(out_obj) <- "scf_mean"
  return(out_obj)
}

#' @export
print.scf_mean <- function(x, ...) {
  dollar_label <- if (isTRUE(attr(x, "nominal")))
    sprintf(" (nominal %d dollars)", x$aux$year) else ""
  cat(sprintf(
    "Multiply-Imputed, Replicate-Weighted Mean Estimate%s\n\n",
    dollar_label
  ))
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

  cat("Pooled Estimates:\n")
  print(
    format(object$results, digits = 4, nsmall = 2),
    row.names = FALSE,
    ...
  )

  cat("\nImplicate-Level Estimates:\n")
  imp_df <- do.call(rbind, object$imps)
  print(
    format(imp_df, digits = 4, nsmall = 2),
    row.names = FALSE
  )

  invisible(object)
}
