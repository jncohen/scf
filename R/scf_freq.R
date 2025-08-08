#' Tabulate a Discrete Variable from SCF Microdata
#'
#' Computes weighted proportions and standard errors for a categorical variable
#' in multiply-imputed SCF data, optionally stratified by a grouping variable.
#' Proportions and standard errors are computed
#' separately within each implicate using `svymean()`, then averaged across
#' implicates using SCF-recommended pooling logic. Group-wise frequencies are
#' supported, but users may find the features of [scf_xtab()] to be more useful.
#'
#' @description
#' This function estimates the relative frequency (proportion) of each category
#' in a discrete variable from the SCF public-use microdata.  Use this function
#' to discern the univariate distribution of a discrete variable.
#'
#'
#' @section Details:
#' Proportions are estimated within each implicate using `survey::svymean()`, then pooled using the standard MI
#' formula for proportions. When a grouping variable is provided via `by`, estimates are produced separately for each
#' group-category combination. Results may be scaled to percentages using the `percent` argument.
#'
#' Estimates are pooled using the standard formula:
#' - The mean of implicate-level proportions is the point estimate
#' - The standard error reflects both within-implicate variance and across-implicate variation
#'
#' Unlike means or model parameters, category proportions do not use Rubin's full combination rules (e.g., degrees of freedom).
#'
#' @param scf A `scf_mi_survey` object created by [scf_load()]. Must contain five replicate-weighted implicates.
#' @param var A one-sided formula specifying a categorical variable (e.g., `~racecl`).
#' @param by Optional one-sided formula specifying a discrete grouping variable (e.g., `~own`).
#' @param percent Logical. If `TRUE` (default), scales results and standard errors to percentages.
#'
#' @return A list of class `"scf_freq"` with:
#' \describe{
#'   \item{results}{Pooled category proportions and standard errors, by group if specified.}
#'   \item{imps}{A named list of implicate-level proportion estimates.}
#'   \item{aux}{Metadata about the variable and grouping structure.}
#' }
#'
#'
#' @seealso [scf_xtab()], [scf_plot_dist()]]
#'
#' @examples
#' \donttest{
#' # Load bundled mock data (for demonstration only — not real SCF data)
#' scf2022 <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))

#' # For real analysis, use:
#' # scf_download(2022); scf2022 <- scf_load(2022)
#'
#'
#' # Proportions by race
#' scf_freq(scf2022, ~own)
#'
#' # Cross-tabulate race by homeownership
#' scf_freq(scf2022, ~own, by = ~racecl)
#' }
#'
#' @export

scf_freq <- function(scf, var, by = NULL, percent = TRUE) {
  if (!inherits(scf, "scf_mi_survey") ||
      !is.list(scf$mi_design) ||
      !all(sapply(scf$mi_design, inherits, "svyrep.design"))) {
    stop("Input must be an 'scf_mi_survey' object with valid multiply-imputed designs.")
  }

  if (isTRUE(attr(scf, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }

  varname <- all.vars(var)[1]
  byname <- if (!is.null(by)) all.vars(by)[1] else NULL

  # Ensure discrete variables
  check_discrete <- function(vname) {
    v <- scf$mi_design[[1]]$variables[[vname]]
    if (is.numeric(v) && length(unique(v)) > 25) {
      stop(sprintf("Variable '%s' appears continuous. Use only discrete variables.", vname))
    }
  }
  check_discrete(varname)
  if (!is.null(byname)) check_discrete(byname)

  designs <- scf$mi_design
  imp_out <- vector("list", length(designs))
  names(imp_out) <- paste0("imp", seq_along(designs))

  for (i in seq_along(designs)) {
    d <- designs[[i]]
    d$variables[[varname]] <- factor(d$variables[[varname]])
    if (!is.null(byname)) d$variables[[byname]] <- factor(d$variables[[byname]])

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
      levels_g <- levels(d$variables[[byname]])
      df <- do.call(rbind, lapply(levels_g, function(g) {
        dsub <- subset(d, d$variables[[byname]] == g)
        est <- try(survey::svymean(as.formula(paste0("~", varname)), dsub), silent = TRUE)
        if (inherits(est, "try-error")) return(NULL)
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

  long <- do.call(rbind, Filter(Negate(is.null), imp_out))
  combos <- unique(long[, c("group", "category")])
  pooled <- lapply(seq_len(nrow(combos)), function(k) {
    g <- combos$group[k]
    c <- combos$category[k]
    subdf <- subset(long,
                    (is.na(group) & is.na(g) | group == g) & category == c)
    m <- length(unique(subdf$implicate))
    qbar <- mean(subdf$est)
    ubar <- mean(subdf$var)
    b <- var(subdf$est)
    se <- sqrt(ubar + (1 + 1/m) * b)

    data.frame(
      group = g,
      category = c,
      proportion = if (percent) 100 * qbar else qbar,
      se_proportion = if (percent) 100 * se else se,
      stringsAsFactors = FALSE
    )
  })

  out <- list(
    results = do.call(rbind, pooled),
    imps = setNames(imp_out, paste0("imp", seq_along(imp_out))),
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
