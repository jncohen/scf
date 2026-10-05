#' Estimate a Ratio of Totals in SCF Microdata
#'
#' Estimates the ratio of two population totals, overall or by group.
#'
#' @details
#' Use it for aggregate shares like those in the Fed's Bulletin, such as
#' financial assets as a share of all assets. An estimate of 0.25 means the
#' numerator total is a quarter of the denominator total. This is not the mean
#' of household ratios.
#'
#' The ratio is `sum(w * num) / sum(w * den)`, estimated in each implicate
#' with [survey::svyratio()] and pooled (see [scf_variance]).
#'
#' @param scf A `scf_mi_survey` object created by [scf_load()].
#' @param num A one-sided formula for the numerator (e.g., `~wageinc`).
#' @param den A one-sided formula for the denominator (e.g., `~income`).
#' @param by Optional one-sided formula for a grouping variable.
#' @param variance Variance method: `"fed"` (default) or `"rubin"`. See [scf_variance].
#'
#' @return An object of class `"scf_ratio"` with:
#' \describe{
#'   \item{results}{Pooled ratio, standard error, and range across implicates, one row per group.}
#'   \item{imps}{Implicate-level estimates.}
#'   \item{aux}{Variable names and survey year.}
#' }
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("ratio_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: share of all income received by homeowners
#' scf2022 <- scf_update(scf2022, owner_income = income * (own == 1))
#' scf_ratio(scf2022, ~owner_income, ~income)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @seealso [scf_mean()], [scf_variance]
#' @export
scf_ratio <- function(scf, num, den, by = NULL,
                      variance = getOption("scf.variance", "fed")) {
  if (!inherits(scf, "scf_mi_survey")) {
    stop("Input must be an 'scf_mi_survey' object.", call. = FALSE)
  }
  if (!inherits(num, "formula") || !inherits(den, "formula")) {
    stop("`num` and `den` must be one-sided formulas, e.g. ~wageinc.", call. = FALSE)
  }
  variance <- .scf_variance_method(variance)

  .scf_check_na(scf$mi_design, num, den, by)

  designs <- scf$mi_design
  nimp <- length(designs)

  ratio_one <- function(d) {
    r <- survey::svyratio(num, den, d)
    c(est = as.numeric(coef(r)), var = as.numeric(survey::SE(r))^2)
  }

  if (is.null(by)) {
    groups <- "All"
    per <- lapply(designs, function(d) list(ratio_one(d)))
  } else {
    byname <- all.vars(by)[1]
    groups <- .scf_group_levels(designs, byname)
    per <- lapply(designs, function(d) {
      g_var <- factor(d$variables[[byname]], levels = groups)
      lapply(groups, function(g) {
        keep <- !is.na(g_var) & g_var == g
        if (!any(keep)) return(NULL)
        ratio_one(d[keep, ])
      })
    })
  }

  out <- do.call(rbind, lapply(seq_along(groups), function(j) {
    est <- sapply(per, function(x) if (is.null(x[[j]])) NA_real_ else x[[j]][["est"]])
    v <- sapply(per, function(x) if (is.null(x[[j]])) NA_real_ else x[[j]][["var"]])
    pooled <- .scf_pool(est, v, variance)
    data.frame(
      group = groups[j],
      numerator = deparse(num[[2]]),
      denominator = deparse(den[[2]]),
      estimate = pooled$estimate,
      se = pooled$se,
      min = min(est, na.rm = TRUE),
      max = max(est, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(by)) out$group <- NULL

  imps <- lapply(seq_len(nimp), function(i) {
    do.call(rbind, lapply(seq_along(groups), function(j) {
      x <- per[[i]][[j]]
      if (is.null(x)) return(NULL)
      data.frame(implicate = i, group = groups[j], estimate = x[["est"]],
                 se = sqrt(x[["var"]]), stringsAsFactors = FALSE)
    }))
  })
  names(imps) <- paste0("imp", seq_len(nimp))

  structure(
    list(results = out, imps = imps,
         aux = list(num = deparse(num[[2]]), den = deparse(den[[2]]),
                    byname = if (is.null(by)) NULL else all.vars(by)[1],
                    year = scf$year)),
    class = "scf_ratio"
  )
}

#' @export
print.scf_ratio <- function(x, ...) {
  cat("SCF Ratio Estimate:", x$aux$num, "/", x$aux$den, "\n\n")
  print(x$results, row.names = FALSE, ...)
  invisible(x)
}

#' @export
summary.scf_ratio <- function(object, ...) {
  cat("Summary of SCF Ratio Estimate:", object$aux$num, "/", object$aux$den, "\n\n")
  cat("Pooled Estimates:\n")
  print(object$results, row.names = FALSE, ...)
  cat("\nImplicate-Level Estimates:\n")
  print(do.call(rbind, object$imps), row.names = FALSE)
  invisible(object)
}
