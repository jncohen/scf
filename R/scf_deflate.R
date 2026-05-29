# CPI-U-RS September values (x100, no decimal) for each SCF survey year.
# Source: Federal Reserve Board SCF Bulletin SAS macro (bulletin.macro.txt).
# See: https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm
.scf_cpi_urs <- c(
  `1989` = 1898, `1992` = 2112, `1995` = 2261, `1998` = 2400,
  `2001` = 2614, `2004` = 2785, `2007` = 3058, `2010` = 3204,
  `2013` = 3438, `2016` = 3548, `2019` = 3775, `2022` = 4376
)

.scf_deflation_factor <- function(survey_year, from_year = 2022) {
  sy <- as.character(survey_year)
  fy <- as.character(from_year)
  valid <- names(.scf_cpi_urs)
  
  if (!sy %in% valid)
    stop(sprintf(
      "Survey year %s not in CPI table. Valid SCF years: %s.",
      sy, paste(valid, collapse = ", ")
    ), call. = FALSE)
  
  if (!fy %in% valid)
    stop(sprintf(
      "from_year %s not in CPI table. Valid SCF years: %s.",
      fy, paste(valid, collapse = ", ")
    ), call. = FALSE)
  
  unname(.scf_cpi_urs[sy] / .scf_cpi_urs[fy])
}


#' Convert SCF Dollar Estimates to Nominal Survey-Year Dollars
#'
#' Converts dollar-valued SCF estimates from real \code{from_year} dollars to
#' nominal survey-year dollars. By default, \code{from_year = 2022}, because
#' the Federal Reserve Summary Extract variables merged by \code{scf_download()}
#' are already inflation-adjusted to 2022 dollars.
#'
#' The function multiplies dollar-valued point estimates and standard errors by
#' the CPI-U-RS conversion factor \code{CPI[survey_year] / CPI[from_year]}.
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
#'
#' \code{scf_freq}, \code{scf_xtab}, and \code{scf_prop_test} return
#' proportions and are not supported. \code{scf_corr} returns a dimensionless
#' coefficient and is not supported. Regression functions
#' (\code{scf_ols}, \code{scf_glm}, \code{scf_logit}, \code{scf_quantreg})
#' are not supported because post-hoc dollar conversion is ambiguous when a
#' model mixes dollar and non-dollar variables. For nominal-dollar regression
#' results, convert dollar-valued variables upstream with \code{scf_update()}
#' before fitting.
#'
#' \strong{Income note:} SCF income is measured for the prior calendar year.
#' A 2019 survey income estimate refers to 2018 income, while this function uses
#' the 2019 SCF CPI-U-RS value. The result is therefore an approximate nominal
#' conversion for income, not an exact prior-calendar-year conversion.
#'
#' This function is experimental. Validation tests indicate that it closely
#' reproduces official nominal-dollar SCF comparison figures, with remaining
#' differences generally small and attributable to rounding, CPI vintage, or
#' source-table conventions rather than the scalar conversion itself.
#'
#' @section Conditions:
#' \describe{
#'   \item{Warning — repeated deflation}{If the input object has already been
#'     deflated (\code{attr(x, "deflated")} is \code{TRUE}), \code{scf_deflate()}
#'     issues a warning and proceeds. Applying the function twice compounds the
#'     CPI adjustment and produces incorrect results. Check
#'     \code{attr(result, "deflated")} and \code{attr(result, "from_year")}
#'     before calling.}
#'   \item{Error — stale result object}{If the result object does not carry a
#'     survey year (\code{x$aux$year} is \code{NULL}), the function stops with
#'     a message asking you to re-run the originating function under the current
#'     package version.}
#'   \item{Error — unsupported class}{Passing an object of an unsupported class
#'     stops with a message listing the supported classes: \code{scf_mean},
#'     \code{scf_median}, \code{scf_percentile}, and \code{scf_ttest}.}
#'   \item{Error — year not in CPI table}{If \code{from_year} is not one of
#'     the valid triennial SCF survey years, the function stops and lists the
#'     valid options.}
#' }
#'
#' @param x An object of class \code{scf_mean}, \code{scf_median},
#'   \code{scf_percentile}, or \code{scf_ttest}.
#' @param from_year Integer. Year whose real-dollar units are assumed for the
#'   input estimate. Defaults to \code{2022}.
#'
#' @return The input object with dollar estimates, standard errors, and
#'   confidence intervals rescaled to nominal survey-year dollars. Attributes
#'   \code{"deflated"} and \code{"from_year"} are set on the returned object.
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
#' # Deflate a mean estimate to nominal survey-year dollars
#' result <- scf_mean(scf2022, ~networth)
#' result_nominal <- scf_deflate(result, from_year = 2022)
#'
#' # Works with median and percentile results
#' med <- scf_median(scf2022, ~networth)
#' med_nominal <- scf_deflate(med, from_year = 2022)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @seealso [scf_mean()], [scf_median()], [scf_percentile()], [scf_ttest()],
#'   [scf_update()]
#'
#' @export
scf_deflate <- function(x, from_year = 2022) {
  UseMethod("scf_deflate")
}

#' @export
scf_deflate.default <- function(x, from_year = 2022) {
  stop(sprintf(
    paste0(
      "scf_deflate() does not support objects of class '%s'.\n",
      "Supported classes: scf_mean, scf_median, scf_percentile, scf_ttest."
    ),
    paste(class(x), collapse = "', '")
  ), call. = FALSE)
}

#' @export
scf_deflate.scf_mean <- function(x, from_year = 2022) {
  .scf_deflate_estimate_object(
    x = x,
    from_year = from_year,
    rerun_function = "scf_mean"
  )
}

#' @export
scf_deflate.scf_percentile <- function(x, from_year = 2022) {
  .scf_deflate_estimate_object(
    x = x,
    from_year = from_year,
    rerun_function = "scf_percentile"
  )
}

#' @export
scf_deflate.scf_median <- function(x, from_year = 2022) {
  scf_deflate.scf_percentile(x, from_year = from_year)
}

.scf_deflate_estimate_object <- function(x, from_year, rerun_function) {
  if (isTRUE(attr(x, "deflated"))) {
    warning(
      "This object has already been deflated. ",
      "Applying scf_deflate() again will compound the adjustment.",
      call. = FALSE
    )
  }
  
  survey_year <- x$aux$year
  
  if (is.null(survey_year)) {
    stop(
      "Survey year not found in result object. ",
      "Re-run ", rerun_function, "() with the current package version.",
      call. = FALSE
    )
  }
  
  f <- .scf_deflation_factor(survey_year, from_year)
  
  for (col in c("estimate", "se", "min", "max")) {
    if (col %in% names(x$results)) {
      x$results[[col]] <- x$results[[col]] * f
    }
  }
  
  x$imps <- lapply(x$imps, function(df) {
    for (col in c("estimate", "se")) {
      if (col %in% names(df)) {
        df[[col]] <- df[[col]] * f
      }
    }
    df
  })
  
  attr(x, "deflated") <- TRUE
  attr(x, "from_year") <- as.integer(from_year)
  
  x
}

#' @export
scf_deflate.scf_ttest <- function(x, from_year = 2022) {
  if (isTRUE(attr(x, "deflated"))) {
    warning(
      "This object has already been deflated. ",
      "Applying scf_deflate() again will compound the adjustment.",
      call. = FALSE
    )
  }
  
  survey_year <- x$aux$year
  
  if (is.null(survey_year)) {
    stop(
      "Survey year not found in result object. ",
      "Re-run scf_ttest() with the current package version.",
      call. = FALSE
    )
  }
  
  f <- .scf_deflation_factor(survey_year, from_year)
  
  for (col in c("estimate", "std.error", "conf.low", "conf.high")) {
    if (col %in% names(x$results)) {
      x$results[[col]] <- x$results[[col]] * f
    }
  }
  
  if (!is.null(x$means) && "mean" %in% names(x$means)) {
    x$means$mean <- x$means$mean * f
  }
  
  if (!is.null(x$fit$null.value)) {
    x$fit$null.value <- x$fit$null.value * f
  }
  
  attr(x, "deflated") <- TRUE
  attr(x, "from_year") <- as.integer(from_year)
  
  x
}
