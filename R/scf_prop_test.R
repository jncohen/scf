#' Test a Proportion in SCF Data
#'
#' @description
#' Tests a binary variable's proportion against a null hypothesis (one-sample),
#' or compares proportions across two groups (two-sample). Supports two-sided,
#' less-than, or greater-than alternatives. 
#'
#' @param design A `scf_mi_survey` object created by [scf_load()]. Must contain replicate-weighted implicates.
#' @param var A one-sided formula indicating a binary variable (e.g., `~rich`).
#' @param group Optional one-sided formula indicating a binary grouping variable (e.g., `~female`). If omitted, a one-sample test is performed.
#' @param p Null hypothesis value. Defaults to `0.5` for one-sample, `0` for two-sample tests.
#' @param alternative Character. One of `"two.sided"` (default), `"less"`, or `"greater"`.
#' @param conf.level Confidence level for the confidence interval. Default is `0.95`.
#'
#' @return An object of class `"scf_prop_test"` with:
#' \describe{
#'   \item{results}{A data frame with the pooled estimate, standard error, z-statistic, p-value, confidence interval, and significance stars.}
#'   \item{proportions}{(Only in two-sample tests) A data frame of pooled proportions by group.}
#'   \item{fit}{A list describing the method, null value, alternative hypothesis, and confidence level.}
#' }
#'
#' @section Statistical Notes:
#' Proportions are computed in each implicate using weighted means, and variances are approximated under the binomial model.
#' Rubin’s Rules are applied to pool point estimates and standard errors. For pooling details, see [scf_MIcombine()].
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td  <- tempdir()
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
#' unlink("scf2022.rds", force = TRUE)
#'
#' 
#' @seealso [scf_ttest()], [scf_mean()], [scf_MIcombine()]
#'
#' @export


scf_prop_test <- function(design, var, group = NULL, p = 0.5,
                          alternative = c("two.sided", "less", "greater"),
                          conf.level = 0.95) {

  stopifnot(inherits(design, "scf_mi_survey"))
  stopifnot(inherits(var, "formula"))
  alternative <- match.arg(alternative)
  varname <- all.vars(var)[1]

  if (any(sapply(design$mi_design, function(d) {
    v <- d$variables[[varname]]
    !is.logical(v) && !all(v %in% c(0, 1, NA))
  }))) {
    stop("Test variable must be logical or 0/1 coded.")
  }

  if (isTRUE(attr(design, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }


  if (is.null(group)) {
    # One-sample
    props <- sapply(design$mi_design, function(d) {
      x <- d$variables[[varname]]
      w <- weights(d, type = "sampling")
      valid <- !is.na(x)
      x <- x[valid]
      w <- w[valid]
      p_hat <- weighted.mean(x, w)
      var_hat <- p_hat * (1 - p_hat) / sum(w)
      c(p_hat = p_hat, var = var_hat)
    })
    est <- mean(props["p_hat", ])
    within <- mean(props["var", ])
    between <- var(props["p_hat", ])
    total_var <- within + (1 + 1 / length(design$mi_design)) * between
    se <- sqrt(total_var)
    zval <- (est - p) / se
    pval <- switch(alternative,
                   "two.sided" = 2 * pnorm(-abs(zval)),
                   "less" = pnorm(zval),
                   "greater" = 1 - pnorm(zval)
    )
    ci <- switch(alternative,
                 "two.sided" = est + c(-1, 1) * qnorm(1 - (1 - conf.level) / 2) * se,
                 "less" = c(-Inf, est + qnorm(conf.level) * se),
                 "greater" = c(est - qnorm(conf.level) * se, Inf)
    )
    prop_df <- NULL

  } else {
    # Two-sample
    stopifnot(inherits(group, "formula"))
    groupname <- all.vars(group)[1]

    levels_detected <- levels(factor(design$mi_design[[1]]$variables[[groupname]]))
    if (length(levels_detected) != 2) {
      stop("Grouping variable must have exactly two levels.")
    }

    stats <- lapply(design$mi_design, function(d) {
      df <- d$variables
      w <- weights(d, type = "sampling")
      g <- factor(df[[groupname]], levels = levels_detected)
      x <- df[[varname]]
      valid <- !is.na(x) & !is.na(g)
      x <- x[valid]
      g <- g[valid]
      w <- w[valid]

      if (length(x) == 0) return(NULL)

      means <- tapply(seq_along(x), g, function(idx) weighted.mean(x[idx], w[idx]))
      ns <- tapply(seq_along(x), g, function(idx) sum(w[idx]))
      vars <- means * (1 - means) / ns

      if (anyNA(means) || anyNA(vars) || length(means) != 2 || length(vars) != 2) return(NULL)

      c(p1 = unname(means[1]), p2 = unname(means[2]), diff = unname(means[1] - means[2]), var = unname(sum(vars)))

    })

    stats <- Filter(Negate(is.null), stats)
    if (length(stats) < 2) stop("Too few valid implicates.")

    ests <- do.call(rbind, stats)
    est <- mean(ests[, "diff"])
    within <- mean(ests[, "var"])
    between <- var(ests[, "diff"])
    total_var <- within + (1 + 1 / length(ests)) * between
    se <- sqrt(total_var)
    zval <- (est - p) / se
    pval <- switch(alternative,
                   "two.sided" = 2 * pnorm(-abs(zval)),
                   "less" = pnorm(zval),
                   "greater" = 1 - pnorm(zval)
    )
    ci <- switch(alternative,
                 "two.sided" = est + c(-1, 1) * qnorm(1 - (1 - conf.level) / 2) * se,
                 "less" = c(-Inf, est + qnorm(conf.level) * se),
                 "greater" = c(est - qnorm(conf.level) * se, Inf)
    )
    prop_df <- data.frame(
      group = levels_detected,
      proportion = c(mean(ests[, "p1"]), mean(ests[, "p2"]))
    )
  }

  results <- data.frame(
    estimate = est,
    std.error = se,
    z.value = zval,
    p.value = pval,
    conf.low = ci[1],
    conf.high = ci[2],
    stars = cut(pval,
                breaks = c(-Inf, 0.001, 0.01, 0.05, 0.10, Inf),
                labels = c("***", "**", "*", "^", ""), right = FALSE
    )
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

