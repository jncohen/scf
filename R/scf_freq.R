#' Estimate the Frequencies of a Discrete Variable from SCF Microdata
#'
#' Estimates the share of households in each category of a discrete variable,
#' overall or by group.
#'
#' @details
#' Use it to describe how households divide across categories, such as
#' education levels. Each estimate is the share of households in a category;
#' with `by`, shares sum to 1 (or 100) within each group. A share plus or
#' minus two standard errors gives a rough 95 percent range. No tests are
#' reported. For two-way tables, use [scf_xtab()].
#'
#' Proportions are estimated in each implicate with `survey::svymean()` and
#' pooled across implicates (see [scf_variance]).
#'
#' @param scf A `scf_mi_survey` object created by [scf_load()]. Must contain five replicate-weighted implicates.
#' @param var A one-sided formula specifying a categorical variable (e.g., `~racecl`).
#' @param by Optional one-sided formula specifying a discrete grouping variable (e.g., `~own`).
#' @param percent Logical. If `TRUE` (default), scales results and standard errors to percentages.
#' @param variance Variance method: `"fed"` (default) or `"rubin"`. See [scf_variance].
#'
#' @return A list of class `"scf_freq"` with:
#' \describe{
#'   \item{results}{Pooled category proportions and standard errors, by group if specified.}
#'   \item{imps}{A named list of implicate-level proportion estimates.}
#'   \item{aux}{Metadata about the variable and grouping structure.}
#' }
#'
#'
#' @seealso [scf_xtab()], [scf_plot_dist()]
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("freq_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Proportions of homeownership
#' scf_freq(scf2022, ~own)
#'
#' # Example for real analysis: Homeownership proportions by education
#' scf_freq(scf2022, ~own, by = ~edcl)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @export
scf_freq <- function(scf, var, by = NULL, percent = TRUE,
                     variance = getOption("scf.variance", "fed")) {
  variance <- .scf_variance_method(variance)
  if (!inherits(scf, "scf_mi_survey") ||
      !is.list(scf$mi_design) ||
      !all(sapply(scf$mi_design, inherits, "svyrep.design"))) {
    stop("Input must be an 'scf_mi_survey' object with valid multiply-imputed designs.")
  }

  varname <- all.vars(var)[1]
  byname <- if (!is.null(by)) all.vars(by)[1] else NULL
  .scf_check_na(scf$mi_design, var, by)

  check_discrete <- function(vname) {
    v <- scf$mi_design[[1]]$variables[[vname]]
    if (is.numeric(v) && length(unique(v)) > 25) {
      stop(sprintf("Variable '%s' appears continuous. Use only discrete variables.", vname))
    }
  }
  check_discrete(varname)
  if (!is.null(byname)) check_discrete(byname)

  designs <- scf$mi_design
  var_levels <- .scf_group_levels(designs, varname)
  by_levels <- if (!is.null(byname)) .scf_group_levels(designs, byname) else NULL
  imp_out <- vector("list", length(designs))
  names(imp_out) <- paste0("imp", seq_along(designs))

  for (i in seq_along(designs)) {
    d <- designs[[i]]
    d$variables[[varname]] <- factor(d$variables[[varname]], levels = var_levels)
    if (!is.null(byname)) d$variables[[byname]] <- factor(d$variables[[byname]], levels = by_levels)

    if (is.null(byname)) {
      est <- survey::svymean(as.formula(paste0("~", varname)), d)
      labs <- gsub(paste0("^", varname), "", names(coef(est)))
      df <- data.frame(
        implicate = i,
        group = NA,
        category = labs,
        est = as.numeric(coef(est)),
        var = diag(vcov(est)),
        stringsAsFactors = FALSE
      )
    } else {
      df <- do.call(rbind, lapply(by_levels, function(g) {
        keep <- !is.na(d$variables[[byname]]) & d$variables[[byname]] == g
        if (!any(keep)) return(NULL)
        est <- survey::svymean(as.formula(paste0("~", varname)), d[keep, ])
        labs <- gsub(paste0("^", varname), "", names(coef(est)))
        data.frame(
          implicate = i,
          group = g,
          category = labs,
          est = as.numeric(coef(est)),
          var = diag(vcov(est)),
          stringsAsFactors = FALSE
        )
      }))
    }
    imp_out[[i]] <- df
  }

  long <- do.call(rbind, imp_out)
  combos <- unique(long[, c("group", "category")])
  pooled <- lapply(seq_len(nrow(combos)), function(k) {
    g <- combos$group[k]
    cat_k <- combos$category[k]
    if (is.na(g)) {
      in_group <- is.na(long$group)
    } else {
      in_group <- !is.na(long$group) & long$group == g
    }
    subdf <- long[in_group & long$category == cat_k, ]
    subdf <- subdf[order(subdf$implicate), ]
    pooled_k <- .scf_pool(subdf$est, subdf$var, variance)
    qbar <- pooled_k$estimate
    se <- pooled_k$se

    data.frame(
      group = g,
      category = cat_k,
      proportion = if (percent) 100 * qbar else qbar,
      se_proportion = if (percent) 100 * se else se,
      stringsAsFactors = FALSE
    )
  })

  results <- do.call(rbind, pooled)
  if (is.null(byname)) {
    results$group <- NULL
    imp_out <- lapply(imp_out, function(df) {
      df$group <- NULL
      df
    })
  }

  out <- list(
    results = results,
    imps = imp_out,
    aux = list(variable = varname, group = byname)
  )
  class(out) <- "scf_freq"
  return(out)
}

#' @export
print.scf_freq <- function(x, ...) {
  cat("SCF Frequency Table (Pooled Results)\n\n")
  print(x$results, row.names = FALSE)
  invisible(x)
}

#' @export
summary.scf_freq <- function(object, ...) {
  cat("Summary of SCF Frequency Analysis\n")
  cat("Variable:", object$aux$variable, "\n")
  if (!is.null(object$aux$group)) cat("Grouped by:", object$aux$group, "\n")
  cat("\nPooled Estimates:\n")
  print(object$results, row.names = FALSE)

  cat("\nIndividual Implicate Results:\n")
  for (i in seq_along(object$imps)) {
    cat("\nImplicate", i, ":\n")
    print(object$imps[[i]], row.names = FALSE)
  }
  invisible(object)
}
