#' Extract Implicate-Level Estimates from SCF Results
#'
#' @description
#' Returns implicate-level outputs from SCF result objects produced by functions
#' in the `scf` suite. Supports result objects containing implicate-level data
#' frames, `svystat` summaries, or `svyglm` model fits.
#'
#' @param x A result object containing implicate-level estimates (e.g., from scf_mean, scf_ols).
#' @param long Logical. If TRUE, returns stacked data frame. If FALSE, returns list.
#'
#' @section Usage:
#' This function allows users to inspect how estimates vary across the SCF’s five implicates,
#' which is important for diagnostics, robustness checks, and transparent reporting.
#'
#' For example: 
#' ```r
#' scf_implicates(scf_mean(scf2022, ~income))
#' scf_implicates(scf_ols(scf2022, networth ~ age + income), long = TRUE)
#' ```
#' @return A list of implicate-level data frames, or a single stacked data frame if `long = TRUE`.
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td  <- tempdir()
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Extract implicate-level results
#' out <- scf_freq(scf2022, ~own)
#' scf_implicates(out, long = TRUE)
#' 
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink("scf2022.rds", force = TRUE)
#'
#' @importFrom stats coef vcov confint
#' @importFrom survey SE cv
#' @export
scf_implicates <- function(x, long = FALSE) {
  imps <- x$imps
  if (is.null(imps)) stop("No implicate-level estimates found in object.")

  # If all implicates are data.frames (e.g., from scf_freq, scf_xtab)
  if (all(sapply(imps, is.data.frame))) {
    out <- lapply(seq_along(imps), function(i) {
      df <- imps[[i]]
      df$implicate <- i
      # Add basic diagnostics if they exist
      if (all(c("est", "var") %in% names(df))) {
        df$estimate <- df$est
        df$se <- sqrt(df$var)
        df$lower <- df$estimate - 1.96 * df$se
        df$upper <- df$estimate + 1.96 * df$se
        df$cv <- df$se / abs(df$estimate)
      }
      df
    })
    return(if (long) do.call(rbind, out) else out)
  }

  # If all implicates are svystat-like objects
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

  # If all implicates are svyglm regression fits
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
        cv = ses / abs(coefs)
      )
    })
    return(if (long) do.call(rbind, out) else out)
  }

  # Fallback: return as-is
  return(imps)
}
