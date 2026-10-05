#' Test a Proportion in SCF Data
#'
#' Tests a proportion against a null value, or the difference between two
#' groups.
#'
#' @details
#' Use it for yes/no variables, such as whether a household owns its home,
#' and to compare two groups, such as homeownership among men and women. The
#' one-sample estimate is the share with the trait; the two-sample estimate is
#' the first group's share minus the second's. A small p-value means a
#' difference this large is unlikely under the null hypothesis; the confidence
#' interval gives the plausible range.
#'
#' The variable must be logical or coded 0/1. Proportions are estimated in
#' each implicate with the replicate weights and pooled (see [scf_variance]).
#' The two-sample test accounts for the covariance between groups.
#'
#' @param design A `scf_mi_survey` object created by [scf_load()]. Must contain replicate-weighted implicates.
#' @param var A one-sided formula indicating a binary variable (e.g., `~rich`).
#' @param group Optional one-sided formula indicating a binary grouping variable (e.g., `~female`). If omitted, a one-sample test is performed.
#' @param p Null hypothesis value. Defaults to `0.5` for one-sample, `0` for two-sample tests.
#' @param alternative Character. One of `"two.sided"` (default), `"less"`, or `"greater"`.
#' @param conf.level Confidence level for the confidence interval. Default is `0.95`.
#' @param variance Variance method: `"fed"` (default) or `"rubin"`. See [scf_variance].
#'
#' @return An object of class `"scf_prop_test"` with:
#' \describe{
#'   \item{results}{Estimate, standard error, t-statistic, degrees of freedom, p-value, confidence interval, and stars. For two-sample tests, the estimate is the first group minus the second.}
#'   \item{proportions}{(Only in two-sample tests) A data frame of pooled proportions by group.}
#'   \item{fit}{A list describing the method, null value, alternative hypothesis, and confidence level.}
#' }
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("proptest_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Wrangle data for example
#' scf2022 <- scf_update(scf2022,
#'   rich   = networth > 1e6,
#'   female = factor(hhsex, levels = 1:2, labels = c("Male","Female")),
#'   over50 = age > 50
#' )
#'
#' # Example for real analysis: One-sample test
#' scf_prop_test(scf2022, ~rich, p = 0.10)
#'
#' # Example for real analysis: Two-sample test
#' scf_prop_test(scf2022, ~rich, ~female, alternative = "less")
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#'
#' @seealso [scf_ttest()], [scf_mean()], [scf_MIcombine()]
#'
#' @export
scf_prop_test <- function(design, var, group = NULL, p = NULL,
                          alternative = c("two.sided", "less", "greater"),
                          conf.level = 0.95,
                          variance = getOption("scf.variance", "fed")) {
  variance <- .scf_variance_method(variance)

  stopifnot(inherits(design, "scf_mi_survey"))
  stopifnot(inherits(var, "formula"))
  alternative <- match.arg(alternative)
  if (is.null(p)) p <- if (is.null(group)) 0.5 else 0
  varname <- all.vars(var)[1]
  .scf_check_na(design$mi_design, var, group)

  if (any(sapply(design$mi_design, function(d) {
    v <- .scf_eval(var, d$variables)
    !is.logical(v) && !all(v %in% c(0, 1, NA))
  }))) {
    stop("Test variable must be logical or 0/1 coded.")
  }


  if (is.null(group)) {
    stats <- lapply(design$mi_design, function(d) {
      d$variables$.x <- as.numeric(.scf_eval(var, d$variables))
      m <- survey::svymean(~.x, d, na.rm = TRUE)
      c(est = unname(coef(m)), var = unname(as.matrix(stats::vcov(m))[1, 1]))
    })
    prop_df <- NULL

  } else {
    stopifnot(inherits(group, "formula"))

    levels_detected <- levels(factor(.scf_eval(group, design$mi_design[[1]]$variables)))
    if (length(levels_detected) != 2) {
      stop("Grouping variable must have exactly two levels.")
    }

    stats <- lapply(seq_along(design$mi_design), function(i) {
      d <- design$mi_design[[i]]
      d$variables$.x <- as.numeric(.scf_eval(var, d$variables))
      d$variables$.g <- factor(.scf_eval(group, d$variables), levels = levels_detected)
      by <- survey::svyby(~.x, ~.g, d, survey::svymean, na.rm = TRUE, covmat = TRUE)
      if (length(coef(by)) != 2) {
        stop(sprintf("Both groups must appear in every implicate; implicate %d has only one.", i),
             call. = FALSE)
      }
      dif <- survey::svycontrast(by, c(1, -1))
      c(p1 = unname(coef(by)[1]), p2 = unname(coef(by)[2]),
        est = unname(coef(dif)), var = unname(as.matrix(stats::vcov(dif))[1, 1]))
    })
  }

  ests <- do.call(rbind, stats)
  pooled <- .scf_pool(ests[, "est"], ests[, "var"], variance)
  est <- pooled$estimate
  se <- pooled$se
  df <- pooled$df
  tval <- (est - p) / se
  pval <- switch(alternative,
                 "two.sided" = 2 * stats::pt(-abs(tval), df),
                 "less" = stats::pt(tval, df),
                 "greater" = 1 - stats::pt(tval, df)
  )
  ci <- switch(alternative,
               "two.sided" = est + c(-1, 1) * stats::qt(1 - (1 - conf.level) / 2, df) * se,
               "less" = c(-Inf, est + stats::qt(conf.level, df) * se),
               "greater" = c(est - stats::qt(conf.level, df) * se, Inf)
  )

  if (!is.null(group)) {
    prop_df <- data.frame(
      group = levels_detected,
      proportion = c(mean(ests[, "p1"]), mean(ests[, "p2"]))
    )
  }

  results <- data.frame(
    estimate = est,
    std.error = se,
    t.value = tval,
    df = df,
    p.value = pval,
    conf.low = ci[1],
    conf.high = ci[2],
    stars = .scf_stars(pval)
  )

  structure(list(
    results = results,
    proportions = prop_df,
    fit = list(
      method = if (is.null(group)) "One-sample proportion test" else "Two-sample proportion test",
      null.value = p,
      conf.level = conf.level,
      alternative = alternative
    )
  ), class = "scf_prop_test")
}
#' @export
print.scf_prop_test <- function(x, digits = 4, ...) {
  cat("\n", x$fit$method, "\n", sep = "")
  cat("Null hypothesis: proportion ",
      if (x$fit$method == "Two-sample proportion test") "difference = " else "= ",
      x$fit$null.value, "\n", sep = "")
  cat("Alternative hypothesis: ", x$fit$alternative, "\n", sep = "")
  cat("Confidence level: ", x$fit$conf.level * 100, "%\n\n", sep = "")

  num_cols <- sapply(x$results, is.numeric)
  rounded <- x$results
  rounded[num_cols] <- round(rounded[num_cols], digits = digits)
  rounded$p.value <- format.pval(x$results$p.value, digits = digits, eps = 1e-4)
  print(rounded, row.names = FALSE)

  if (!is.null(x$proportions)) {
    cat("\nEstimated group proportions:\n")
    prop_rounded <- x$proportions
    num_cols <- sapply(prop_rounded, is.numeric)
    prop_rounded[num_cols] <- round(prop_rounded[num_cols], digits = digits)
    print(prop_rounded, row.names = FALSE)
  }

  invisible(x)
}


#' @export
summary.scf_prop_test <- function(object, ...) {
  print(object, ...)
}
