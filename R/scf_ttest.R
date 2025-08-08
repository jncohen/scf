#' T-Test of Means using SCF Microdata
#'
#' @description
#' Tests whether the mean of a continuous variable differs from a specified
#' value (one-sample), or whether group means differ across a binary factor
#' (two-sample). Estimates and standard errors are computed using `svymean()`
#' within each implicate, then pooled using Rubin’s Rules.  Use this function
#' to test hypotheses about means in the SCF microdata. 
#'
#' @param design A `scf_mi_survey` object created by [scf_load()].
#' @param var A one-sided formula specifying a numeric variable (e.g., `~income`).
#' @param group Optional one-sided formula specifying a binary grouping variable (e.g., `~female`).
#' @param mu Numeric. Null hypothesis value. Default is `0`.
#' @param alternative Character. One of `"two.sided"` (default), `"less"`, or `"greater"`.
#' @param conf.level Confidence level for the confidence interval. Default is `0.95`.
#'
#' @return An object of class `"scf_ttest"` with:
#' \describe{
#'   \item{results}{A data frame with pooled estimate, standard error, t-statistic, degrees of freedom, p-value, and confidence interval.}
#'   \item{means}{Group-specific means (for two-sample tests only).}
#'   \item{fit}{List describing the test type, null hypothesis, confidence level, and alternative.}
#' }
#'
#' @examples
#' \donttest{
#' # Load bundled mock data (for demonstration only — not real SCF data)
#' scf2022 <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))

#' # For real analysis, use:
#' # scf_download(2022); scf2022 <- scf_load(2022)
#'
#' scf2022 <- scf_update(scf2022,
#'   female = factor(hhsex, levels = 1:2, labels = c("Male", "Female")),
#'   over50 = age > 50
#' )
#'
#' # One-sample test
#' scf_ttest(scf2022, ~income, mu = 75000)
#'
#' # Two-sample test
#' scf_ttest(scf2022, ~income, group = ~female)
#' }
#'
#' @seealso [scf_prop_test()], [scf_mean()], [scf_MIcombine()]
#' @export
scf_ttest <- function(design, var, group = NULL, mu = 0,
                      alternative = c("two.sided", "less", "greater"),
                      conf.level = 0.95) {

  stopifnot(inherits(design, "scf_mi_survey"))
  stopifnot(inherits(var, "formula"))

  if (isTRUE(attr(design, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }

  alternative <- match.arg(alternative)
  varname <- all.vars(var)[1]

  # Check variable type
  if (!is.numeric(design$mi_design[[1]]$variables[[varname]])) {
    stop(sprintf("Variable '%s' must be numeric.", varname))
  }

  if (is.null(group)) {
    # One-sample t-test
    stats <- lapply(design$mi_design, function(d) {
      est <- survey::svymean(as.formula(paste0("~", varname)), d)
      data.frame(est = coef(est), se = survey::SE(est))
    })
    stats_df <- do.call(rbind, stats)
    m <- length(stats_df$est)
    qbar <- mean(stats_df$est)
    ubar <- mean(stats_df$se^2)
    b <- var(stats_df$est)
    tvar <- ubar + (1 + 1/m) * b
    se <- sqrt(tvar)
    df <- (m - 1) * (1 + ubar / ((1 + 1/m) * b))^2
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
    # Two-sample t-test
    stopifnot(inherits(group, "formula"))
    groupname <- all.vars(group)[1]
    levels_detected <- levels(factor(design$mi_design[[1]]$variables[[groupname]]))
    if (length(levels_detected) != 2) {
      stop("Grouping variable must have exactly two levels.")
    }

    stats <- lapply(design$mi_design, function(d) {
      d$variables[[groupname]] <- factor(d$variables[[groupname]], levels = levels_detected)
      d1 <- subset(d, d$variables[[groupname]] == levels_detected[1])
      d2 <- subset(d, d$variables[[groupname]] == levels_detected[2])
      est1 <- survey::svymean(as.formula(paste0("~", varname)), d1)
      est2 <- survey::svymean(as.formula(paste0("~", varname)), d2)
      data.frame(
        diff = coef(est1) - coef(est2),
        se = sqrt(survey::SE(est1)^2 + survey::SE(est2)^2),
        m1 = coef(est1),
        m2 = coef(est2)
      )
    })
    stats_df <- do.call(rbind, stats)
    m <- nrow(stats_df)
    qbar <- mean(stats_df$diff)
    ubar <- mean(stats_df$se^2)
    b <- var(stats_df$diff)
    tvar <- ubar + (1 + 1/m) * b
    se <- sqrt(tvar)
    df <- (m - 1) * (1 + ubar / ((1 + 1/m) * b))^2
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
    stars = cut(pval,
                breaks = c(-Inf, 0.001, 0.01, 0.05, 0.10, Inf),
                labels = c("***", "**", "*", "^", ""), right = FALSE)
  )

  structure(list(
    results = results,
    means = means_df,
    fit = list(
      method = if (is.null(group)) "One-sample t-test" else "Two-sample t-test",
      null.value = mu,
      conf.level = conf.level,
      alternative = alternative
    )
  ), class = "scf_ttest")
}

#' @export
print.scf_ttest <- function(x, digits = 4, ...) {
  cat("SCF", x$fit$method, "\n")
  cat("Alternative hypothesis:",
      switch(x$fit$alternative,
             "two.sided" = "mean is not equal to",
             "less" = "mean is less than",
             "greater" = "mean is greater than"),
      x$fit$null.value, "\n\n")

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
  cat(sprintf("t = %.2f, df = %.1f, p = %.4f %s",
              x$results$t.value, x$results$df, x$results$p.value, x$results$stars), "\n")
  cat(sprintf("CI (%.0f%%): [%.2f, %.2f]",
              100 * x$fit$conf.level,
              x$results$conf.low,
              x$results$conf.high), "\n")
  invisible(x)
}


#' @export
summary.scf_ttest <- function(object, ...) {
  cat("SCF", object$fit$method, "\n")
  cat("Null hypothesis: mu =", object$fit$null.value, "\n")
  cat("Alternative hypothesis:", object$fit$alternative, "\n")
  cat(sprintf("Confidence level: %.0f%%\n\n", 100 * object$fit$conf.level))

  if (!is.null(object$means)) {
    cat("Group-specific means:\n")
    print(round(object$means, 2))
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
