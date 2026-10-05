#' T-Test of Means using SCF Microdata
#'
#' Tests a mean against a null value, or the difference between two group
#' means.
#'
#' @details
#' Use it to test a claim about a mean, such as whether mean income differs
#' between two groups. The one-sample estimate is the mean; the two-sample
#' estimate is the first group's mean minus the second's. A small p-value
#' means a difference this large is unlikely under the null hypothesis; the
#' confidence interval gives the plausible range. Means of wealth and income
#' are dominated by the top of the distribution and may not reflect the
#' typical household.
#'
#' Means are estimated in each implicate with `survey::svymean()` and pooled
#' (see [scf_variance]). The two-sample test accounts for the covariance
#' between groups.
#'
#' @param design A `scf_mi_survey` object created by [scf_load()].
#' @param var A one-sided formula specifying a numeric variable (e.g., `~income`).
#' @param group Optional one-sided formula specifying a binary grouping variable (e.g., `~female`).
#' @param mu Numeric. Null hypothesis value. Default is `0`.
#' @param alternative Character. One of `"two.sided"` (default), `"less"`, or `"greater"`.
#' @param conf.level Confidence level for the confidence interval. Default is `0.95`.
#' @param variance Variance method: `"fed"` (default) or `"rubin"`. See [scf_variance].
#'
#' @return An object of class `scf_ttest` with:
#' \describe{
#'   \item{results}{A data frame with pooled estimate, standard error, t-statistic, degrees of freedom, p-value, and confidence interval. For two-sample tests, the estimate is the first group minus the second.}
#'   \item{means}{Group-specific means (for two-sample tests only).}
#'   \item{fit}{List describing the test type, null hypothesis, confidence level, and alternative.}
#' }
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   # Do not implement these lines in real analysis:
#'   # Use functions `scf_download()` and `scf_load()`
#'   td <- tempfile("ttest_")
#'   dir.create(td)
#'
#'   src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#'   file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#'   scf2022 <- scf_load(2022, data_directory = td)
#'
#'   # Wrangle data for example: Derive analysis vars
#'   scf2022 <- scf_update(scf2022,
#'     female = factor(hhsex, levels = 1:2, labels = c("Male", "Female")),
#'     over50 = age > 50
#'   )
#'
#'   # Example for real analysis: One-sample t-test
#'   scf_ttest(scf2022, ~income, mu = 75000)
#'
#'   # Example for real analysis: Two-sample t-test
#'   scf_ttest(scf2022, ~income, group = ~female)
#'
#'   # Do not implement these lines in real analysis: Cleanup for package check
#'   unlink(td, recursive = TRUE, force = TRUE)
#' }
#' }
#'
#' @seealso [scf_prop_test()], [scf_mean()], [scf_MIcombine()]
#' @export
scf_ttest <- function(design, var, group = NULL, mu = 0,
                      alternative = c("two.sided", "less", "greater"),
                      conf.level = 0.95,
                      variance = getOption("scf.variance", "fed")) {
  variance <- .scf_variance_method(variance)

  stopifnot(inherits(design, "scf_mi_survey"))
  stopifnot(inherits(var, "formula"))

  alternative <- match.arg(alternative)
  varname <- all.vars(var)[1]
  .scf_check_na(design$mi_design, var, group)

  if (!is.numeric(.scf_eval(var, design$mi_design[[1]]$variables))) {
    stop(sprintf("Variable '%s' must be numeric.", varname))
  }

  if (is.null(group)) {
    stats <- lapply(design$mi_design, function(d) {
      est <- survey::svymean(var, d)
      data.frame(est = coef(est), se = survey::SE(est))
    })
    stats_df <- do.call(rbind, stats)
    pooled <- .scf_pool(stats_df$est, stats_df$se^2, variance)
    qbar <- pooled$estimate
    se <- pooled$se
    df <- pooled$df
    tval <- (qbar - mu) / se
    pval <- switch(alternative,
                   "two.sided" = 2 * pt(-abs(tval), df),
                   "less" = pt(tval, df),
                   "greater" = 1 - pt(tval, df))
    ci <- switch(alternative,
                 "two.sided" = qbar + c(-1, 1) * qt(1 - (1 - conf.level)/2, df) * se,
                 "less" = c(-Inf, qbar + qt(conf.level, df) * se),
                 "greater" = c(qbar - qt(conf.level, df) * se, Inf))
    means_df <- NULL

  } else {
    stopifnot(inherits(group, "formula"))
    levels_detected <- levels(factor(.scf_eval(group, design$mi_design[[1]]$variables)))
    if (length(levels_detected) != 2) {
      stop("Grouping variable must have exactly two levels.")
    }

    stats <- lapply(seq_along(design$mi_design), function(i) {
      d <- design$mi_design[[i]]
      d$variables$.g <- factor(.scf_eval(group, d$variables), levels = levels_detected)
      by <- survey::svyby(var, ~.g, d,
                          survey::svymean, covmat = TRUE)
      if (length(coef(by)) != 2) {
        stop(sprintf("Both groups must appear in every implicate; implicate %d has only one.", i),
             call. = FALSE)
      }
      dif <- survey::svycontrast(by, c(1, -1))
      data.frame(
        diff = unname(coef(dif)),
        se = unname(sqrt(as.matrix(stats::vcov(dif))[1, 1])),
        m1 = unname(coef(by)[1]),
        m2 = unname(coef(by)[2])
      )
    })
    stats_df <- do.call(rbind, stats)
    pooled <- .scf_pool(stats_df$diff, stats_df$se^2, variance)
    qbar <- pooled$estimate
    se <- pooled$se
    df <- pooled$df
    tval <- (qbar - mu) / se
    pval <- switch(alternative,
                   "two.sided" = 2 * pt(-abs(tval), df),
                   "less" = pt(tval, df),
                   "greater" = 1 - pt(tval, df))
    ci <- switch(alternative,
                 "two.sided" = qbar + c(-1, 1) * qt(1 - (1 - conf.level)/2, df) * se,
                 "less" = c(-Inf, qbar + qt(conf.level, df) * se),
                 "greater" = c(qbar - qt(conf.level, df) * se, Inf))
    means_df <- data.frame(
      group = levels_detected,
      mean = c(mean(stats_df$m1), mean(stats_df$m2))
    )
  }

  results <- data.frame(
    estimate = qbar,
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
    means = means_df,
    fit = list(
      method = if (is.null(group)) "One-sample t-test" else "Two-sample t-test",
      null.value = mu,
      conf.level = conf.level,
      alternative = alternative
    ),
    aux = list(year = design$year, varname = varname)
  ), class = "scf_ttest")
}

#' @export
print.scf_ttest <- function(x, digits = 4, ...) {
  dollar_label <- if (isTRUE(attr(x, "nominal")))
    sprintf(" (nominal %d dollars)", x$aux$year) else ""
  cat(sprintf("SCF %s%s\n", x$fit$method, dollar_label))
  what <- if (is.null(x$means)) "mean" else "difference in means"
  cat("Alternative hypothesis:", what,
      switch(x$fit$alternative,
             "two.sided" = "is not equal to",
             "less" = "is less than",
             "greater" = "is greater than"),
      format(x$fit$null.value, big.mark = ",", scientific = FALSE), "\n\n")

  if (!is.null(x$means)) {
    cat("Group means:\n")
    rounded_means <- x$means
    num_cols <- sapply(rounded_means, is.numeric)
    rounded_means[num_cols] <- round(rounded_means[num_cols], digits)
    print(rounded_means, row.names = FALSE)
    cat("\n")
  }

  cat(sprintf("Estimate: %.2f", x$results$estimate), "\n")
  cat(sprintf("Standard Error: %.2f", x$results$std.error), "\n")
  p_txt <- format.pval(x$results$p.value, digits = 4, eps = 1e-4)
  if (!startsWith(p_txt, "<")) p_txt <- paste("=", p_txt)
  cat(sprintf("t = %.2f, df = %.1f, p %s %s",
              x$results$t.value, x$results$df, p_txt, x$results$stars), "\n")
  cat(sprintf("CI (%.0f%%): [%.2f, %.2f]",
              100 * x$fit$conf.level,
              x$results$conf.low,
              x$results$conf.high), "\n")
  invisible(x)
}

#' @export
summary.scf_ttest <- function(object, ...) {
  cat("SCF", object$fit$method, "\n")
  cat("Null hypothesis: mu =", format(object$fit$null.value, big.mark = ",", scientific = FALSE), "\n")
  cat("Alternative hypothesis:", object$fit$alternative, "\n")
  cat(sprintf("Confidence level: %.0f%%\n\n", 100 * object$fit$conf.level))

  if (!is.null(object$means)) {
    cat("Group-specific means:\n")
    means <- object$means
    num_cols <- sapply(means, is.numeric)
    means[num_cols] <- round(means[num_cols], 2)
    print(means, row.names = FALSE)
    cat("\n")
  }

  cat("Test results:\n")
  out <- object$results
  out$p.value <- format.pval(out$p.value, digits = 4, eps = .0001)
  out$estimate <- round(out$estimate, 2)
  out$std.error <- round(out$std.error, 2)
  out$t.value <- round(out$t.value, 2)
  out$conf.low <- round(out$conf.low, 2)
  out$conf.high <- round(out$conf.high, 2)
  print(out[, c("estimate", "std.error", "t.value", "df", "p.value", "conf.low", "conf.high", "stars")],
        row.names = FALSE)
  invisible(object)
}
