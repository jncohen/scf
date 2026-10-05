#' Extract Implicate-Level Estimates from SCF Results
#'
#' Returns the five implicate-level estimates behind an `scf` result.
#'
#' @details
#' Use it to check how much imputation affects a result. Implicate estimates
#' close together mean imputation adds little uncertainty; estimates far apart
#' mean the result depends on imputed values, and its standard error is
#' larger. Works on results from the descriptive functions and the models.
#'
#' @param x A result object containing implicate-level estimates. Three types are supported:
#'   \describe{
#'     \item{Descriptive results}{Objects from [scf_freq()], [scf_mean()], [scf_median()],
#'       and [scf_percentile()]. Also [scf_corr()] (one correlation per implicate) and
#'       [scf_xtab()] (weighted cell counts per implicate).}
#'     \item{Survey statistic results}{Objects whose `$imps` slot contains `svystat`
#'       objects (e.g., raw outputs from `survey::svymean()`).}
#'     \item{Model results}{Objects from [scf_ols()], [scf_glm()], [scf_logit()], and
#'       [scf_quantreg()].}
#'   }
#' @param long Logical. If TRUE, returns stacked data frame. If FALSE, returns list.
#'
#' @return A list of implicate-level data frames, or a single stacked data frame if `long = TRUE`.
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("implicates_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Extract implicate-level results
#' out <- scf_freq(scf2022, ~own)
#' scf_implicates(out, long = TRUE)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @importFrom stats coef vcov confint
#' @importFrom survey SE cv
#' @export
scf_implicates <- function(x, long = FALSE) {
  imps <- if (!is.null(x$imps)) x$imps else x$models
  if (is.null(imps)) stop("No implicate-level estimates found in object.")

  if (is.numeric(imps) && is.null(dim(imps))) {
    out <- lapply(seq_along(imps), function(i) {
      data.frame(implicate = i, estimate = unname(imps[i]))
    })
    return(if (long) do.call(rbind, out) else out)
  }

  if (all(sapply(imps, is.matrix))) {
    out <- lapply(seq_along(imps), function(i) {
      tab <- imps[[i]]
      data.frame(
        implicate = i,
        row = rep(rownames(tab), times = ncol(tab)),
        col = rep(colnames(tab), each = nrow(tab)),
        count = as.vector(tab),
        stringsAsFactors = FALSE
      )
    })
    return(if (long) do.call(rbind, out) else out)
  }

  if (all(sapply(imps, function(m) inherits(m, "rq")))) {
    out <- lapply(seq_along(imps), function(i) {
      coefs <- coef(imps[[i]])
      ses <- if (!is.null(x$imp_vcov)) sqrt(diag(x$imp_vcov[[i]]))[names(coefs)] else NA_real_
      data.frame(
        implicate = i,
        term = names(coefs),
        estimate = unname(coefs),
        se = unname(ses),
        lower = unname(coefs - 1.96 * ses),
        upper = unname(coefs + 1.96 * ses),
        stringsAsFactors = FALSE
      )
    })
    return(if (long) do.call(rbind, out) else out)
  }

  if (all(sapply(imps, is.data.frame))) {
    out <- lapply(seq_along(imps), function(i) {
      df <- imps[[i]]
      df$implicate <- i
      if (all(c("est", "var") %in% names(df))) {
        df$estimate <- df$est
        df$se <- sqrt(df$var)
        df$lower <- df$estimate - 1.96 * df$se
        df$upper <- df$estimate + 1.96 * df$se
        df$cv <- ifelse(df$estimate == 0, NA_real_, df$se / abs(df$estimate))
      }
      df
    })
    return(if (long) do.call(rbind, out) else out)
  }

  if (all(sapply(imps, function(x) inherits(x, "svystat")))) {
    out <- lapply(seq_along(imps), function(i) {
      est <- imps[[i]]
      data.frame(
        implicate = i,
        estimate = coef(est),
        se = tryCatch(survey::SE(est), error = function(e) NA),
        lower = tryCatch(confint(est)[, 1], error = function(e) NA),
        upper = tryCatch(confint(est)[, 2], error = function(e) NA),
        cv = tryCatch(survey::cv(est), error = function(e) NA)
      )
    })
    return(if (long) do.call(rbind, out) else out)
  }

  if (all(sapply(imps, function(x) inherits(x, "svyglm")))) {
    out <- lapply(seq_along(imps), function(i) {
      fit <- imps[[i]]
      coefs <- coef(fit)
      ses <- tryCatch(sqrt(diag(vcov(fit))), error = function(e) NA)
      data.frame(
        implicate = i,
        term = names(coefs),
        estimate = coefs,
        se = ses,
        lower = coefs - 1.96 * ses,
        upper = coefs + 1.96 * ses,
        cv = ifelse(coefs == 0, NA_real_, ses / abs(coefs))
      )
    })
    return(if (long) do.call(rbind, out) else out)
  }

  return(imps)
}
