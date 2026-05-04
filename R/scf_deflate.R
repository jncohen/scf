# CPI-U-RS September values (x100, no decimal) for each SCF survey year.
# Source: Federal Reserve Board SCF Bulletin SAS macro (bulletin.macro.txt).
# See: https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm
.scf_cpi_urs <- c(
  `1989` = 1898, `1992` = 2112, `1995` = 2261, `1998` = 2400,
  `2001` = 2614, `2004` = 2785, `2007` = 3058, `2010` = 3204,
  `2013` = 3438, `2016` = 3548, `2019` = 3775, `2022` = 4376
)

.scf_deflation_factor <- function(survey_year, base_year) {
  sy <- as.character(survey_year)
  by <- as.character(base_year)
  valid <- names(.scf_cpi_urs)
  if (!sy %in% valid)
    stop(sprintf(
      "Survey year %s not in CPI table. Valid SCF years: %s.",
      sy, paste(valid, collapse = ", ")
    ), call. = FALSE)
  if (!by %in% valid)
    stop(sprintf(
      "base_year %s not in CPI table. Valid SCF years: %s.",
      by, paste(valid, collapse = ", ")
    ), call. = FALSE)
  unname(.scf_cpi_urs[by] / .scf_cpi_urs[sy])
}


#' Convert SCF Dollar Estimates to Real Terms
#'
#' Multiplies dollar-valued point estimates and their standard errors by the
#' CPI-U-RS deflation factor \code{CPI[base_year] / CPI[survey_year]},
#' converting nominal survey-year dollars to real dollars of the chosen base
#' year. The CPI-U-RS September values are taken directly from the Federal
#' Reserve Board's SCF Bulletin SAS macro.
#'
#' @details
#' Standard errors rescale correctly under linear multiplication, so both
#' estimates and SEs are multiplied by the same factor. Confidence intervals
#' and group means are also rescaled. The t-statistic, degrees of freedom,
#' and p-value in \code{scf_ttest} results are invariant to this rescaling
#' and are left unchanged.
#'
#' \strong{Supported functions:} \code{scf_mean}, \code{scf_median},
#' \code{scf_percentile}, and \code{scf_ttest} return dollar-valued estimates
#' that transform correctly under scalar multiplication.
#' \code{scf_freq}, \code{scf_xtab}, and \code{scf_prop_test} return
#' proportions and are not supported. \code{scf_corr} returns a dimensionless
#' coefficient and is not supported. The regression functions
#' (\code{scf_ols}, \code{scf_glm}, \code{scf_logit}, \code{scf_quantreg})
#' are not supported because deflating coefficients post hoc is ambiguous
#' when a model mixes dollar and non-dollar variables; for real-dollar
#' regression results, deflate variables upstream with \code{scf_update()}
#' before fitting.
#'
#' \strong{Income note:} SCF income is measured in the prior calendar year,
#' so a mean income estimate from the 2019 survey is in 2018 dollars, not
#' 2019 dollars. \code{scf_deflate()} applies \code{CPI[base] / CPI[2019]},
#' which is a close but not exact conversion for income. This is documented
#' in the Federal Reserve's SAS macro, which applies a separate lag
#' adjustment to income before the main deflation step.
#'
#' @param x An object of class \code{scf_mean}, \code{scf_median},
#'   \code{scf_percentile}, or \code{scf_ttest}.
#' @param base_year Integer. Reference year for real dollars. Must be a valid
#'   SCF survey year (1989--2022, triennial). Default is \code{2022}.
#'
#' @return The input object with dollar estimates, standard errors, and
#'   confidence intervals rescaled to \code{base_year} dollars. Attributes
#'   \code{"deflated"} (logical) and \code{"base_year"} (integer) are set on
#'   the returned object. Calling \code{scf_deflate()} on an already-deflated
#'   object raises a warning.
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("deflate_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Deflate a mean estimate to real 2022 dollars
#' result <- scf_mean(scf2022, ~networth)
#' result_real <- scf_deflate(result, base_year = 2022)
#'
#' # Works with median and percentile results
#' med <- scf_median(scf2022, ~networth)
#' med_real <- scf_deflate(med, base_year = 2022)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @seealso [scf_mean()], [scf_median()], [scf_percentile()], [scf_ttest()],
#'   [scf_update()]
#'
#' @export
scf_deflate <- function(x, base_year = 2022) {
  UseMethod("scf_deflate")
}

#' @export
scf_deflate.default <- function(x, base_year = 2022) {
  stop(sprintf(
    paste0(
      "scf_deflate() does not support objects of class '%s'.\n",
      "Supported classes: scf_mean, scf_median, scf_percentile, scf_ttest."
    ),
    paste(class(x), collapse = "', '")
  ), call. = FALSE)
}

#' @export
scf_deflate.scf_mean <- function(x, base_year = 2022) {
  if (isTRUE(attr(x, "deflated")))
    warning(
      "This object has already been deflated. ",
      "Applying scf_deflate() again will compound the adjustment.",
      call. = FALSE
    )
  survey_year <- x$aux$year
  if (is.null(survey_year))
    stop(
      "Survey year not found in result object. ",
      "Re-run scf_mean() with the current package version.",
      call. = FALSE
    )
  f <- .scf_deflation_factor(survey_year, base_year)

  for (col in c("estimate", "se", "min", "max")) {
    if (col %in% names(x$results))
      x$results[[col]] <- x$results[[col]] * f
  }
  x$imps <- lapply(x$imps, function(df) {
    for (col in c("estimate", "se"))
      if (col %in% names(df)) df[[col]] <- df[[col]] * f
    df
  })
  attr(x, "deflated")  <- TRUE
  attr(x, "base_year") <- as.integer(base_year)
  x
}

#' @export
scf_deflate.scf_percentile <- function(x, base_year = 2022) {
  if (isTRUE(attr(x, "deflated")))
    warning(
      "This object has already been deflated. ",
      "Applying scf_deflate() again will compound the adjustment.",
      call. = FALSE
    )
  survey_year <- x$aux$year
  if (is.null(survey_year))
    stop(
      "Survey year not found in result object. ",
      "Re-run scf_percentile() with the current package version.",
      call. = FALSE
    )
  f <- .scf_deflation_factor(survey_year, base_year)

  for (col in c("estimate", "se", "min", "max")) {
    if (col %in% names(x$results))
      x$results[[col]] <- x$results[[col]] * f
  }
  x$imps <- lapply(x$imps, function(df) {
    for (col in c("estimate", "se"))
      if (col %in% names(df)) df[[col]] <- df[[col]] * f
    df
  })
  attr(x, "deflated")  <- TRUE
  attr(x, "base_year") <- as.integer(base_year)
  x
}

#' @export
scf_deflate.scf_median <- function(x, base_year = 2022) {
  scf_deflate.scf_percentile(x, base_year = base_year)
}

#' @export
scf_deflate.scf_ttest <- function(x, base_year = 2022) {
  if (isTRUE(attr(x, "deflated")))
    warning(
      "This object has already been deflated. ",
      "Applying scf_deflate() again will compound the adjustment.",
      call. = FALSE
    )
  survey_year <- x$aux$year
  if (is.null(survey_year))
    stop(
      "Survey year not found in result object. ",
      "Re-run scf_ttest() with the current package version.",
      call. = FALSE
    )
  f <- .scf_deflation_factor(survey_year, base_year)

  # Scale estimate, SE, and confidence bounds; t, df, p are invariant
  for (col in c("estimate", "std.error", "conf.low", "conf.high")) {
    if (col %in% names(x$results))
      x$results[[col]] <- x$results[[col]] * f
  }
  # Scale group means (two-sample case)
  if (!is.null(x$means) && "mean" %in% names(x$means))
    x$means$mean <- x$means$mean * f
  # Scale the null hypothesis value
  x$fit$null.value <- x$fit$null.value * f

  attr(x, "deflated")  <- TRUE
  attr(x, "base_year") <- as.integer(base_year)
  x
}
