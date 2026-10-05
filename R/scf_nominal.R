#' Convert SCF Dollar Estimates to Nominal Survey-Year Dollars
#'
#' Converts dollar estimates from 2022 dollars to dollars of the survey year.
#'
#' @details
#' Estimates, standard errors, and confidence limits are multiplied by the
#' CPI-U-RS for the survey year divided by that for `from_year`. Works on
#' results from [scf_mean()], [scf_median()], [scf_percentile()], and
#' [scf_ttest()].
#'
#' The Fed's summary variables, such as `income` and `networth`, are in 2022
#' dollars. Raw `x` variables are already nominal. Income in the summary
#' extract is first moved to survey-year dollars, so converted income is in
#' survey-year dollars, as in the Fed's nominal tables. Medians in Tables 1
#' and 4 match the Fed within 0.05 percent for 1989 to 2022.
#'
#' The function stops, unless `force = TRUE`, if the result is for a raw `x`
#' variable or for a variable not known to be in 2022 dollars (the Fed's
#' dollar summary variables and names ending in `"_real"`). Converting twice
#' gives a warning.
#'
#' @param x An object of class `scf_mean`, `scf_median`,
#'   `scf_percentile`, or `scf_ttest`.
#' @param from_year Integer. Year whose real-dollar units are assumed for the
#'   input estimate. Defaults to `2022`.
#' @param force Logical. If `TRUE`, skip the check on the variable.
#'
#' @return The input object with dollar estimates, standard errors, and
#'   confidence intervals rescaled to nominal survey-year dollars. Attributes
#'   `"nominal"` and `"from_year"` are set on the returned object.
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("nominal_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Convert a mean estimate to nominal survey-year dollars
#' result <- scf_mean(scf2022, ~networth)
#' result_nominal <- scf_nominal(result)
#'
#' # Works with median and percentile results
#' med <- scf_median(scf2022, ~networth)
#' med_nominal <- scf_nominal(med)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @seealso [scf_real()], [scf_mean()], [scf_median()], [scf_percentile()],
#'   [scf_ttest()]
#'
#' @references
#' Board of Governors of the Federal Reserve System. SAS macro for SCF
#'   Bulletin variable definitions (bulletin.macro.txt).
#'   <https://www.federalreserve.gov/econres/files/bulletin.macro.txt>
#'
#' U.S. Bureau of Labor Statistics. Consumer Price Index research series
#'   using current methods (CPI-U-RS).
#'   <https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm>
#'
#' @export
scf_nominal <- function(x, from_year = 2022, force = FALSE) {
  UseMethod("scf_nominal")
}

#' @export
scf_nominal.default <- function(x, from_year = 2022, force = FALSE) {
  stop(sprintf(
    paste0(
      "scf_nominal() does not support objects of class '%s'.\n",
      "Supported classes: scf_mean, scf_median, scf_percentile, scf_ttest."
    ),
    paste(class(x), collapse = "', '")
  ), call. = FALSE)
}

#' @export
scf_nominal.scf_mean <- function(x, from_year = 2022, force = FALSE) {
  .scf_nominal_estimate(x, from_year, force, "scf_mean")
}

#' @export
scf_nominal.scf_percentile <- function(x, from_year = 2022, force = FALSE) {
  .scf_nominal_estimate(x, from_year, force, "scf_percentile")
}

#' @export
scf_nominal.scf_median <- function(x, from_year = 2022, force = FALSE) {
  .scf_nominal_estimate(x, from_year, force, "scf_median")
}

#' @export
scf_nominal.scf_ttest <- function(x, from_year = 2022, force = FALSE) {
  f <- .scf_nominal_setup(x, from_year, force, "scf_ttest")

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

  attr(x, "nominal") <- TRUE
  attr(x, "from_year") <- as.integer(from_year)
  x
}

#' Deprecated: use scf_nominal()
#'
#' `scf_deflate()` was renamed [scf_nominal()] in version 1.1.0 and will be
#' removed in a future release.
#'
#' @param x A result from [scf_mean()], [scf_median()], [scf_percentile()],
#'   or [scf_ttest()].
#' @param from_year Passed to [scf_nominal()].
#' @return The result of [scf_nominal()].
#' @keywords internal
#' @export
scf_deflate <- function(x, from_year = 2022) {
  .Deprecated("scf_nominal")
  scf_nominal(x, from_year = from_year)
}

.scf_nominal_setup <- function(x, from_year, force, rerun_function) {
  if (isTRUE(attr(x, "nominal"))) {
    warning(
      "This result is already in nominal dollars. ",
      "Converting it again applies the adjustment twice.",
      call. = FALSE
    )
  }

  if (is.null(x$aux$year) || is.null(x$aux$varname)) {
    stop(
      "Survey year or variable name not found in result object. ",
      "Re-run ", rerun_function, "() with the current package version.",
      call. = FALSE
    )
  }

  vars <- unique(c(x$aux$varname, x$results$variable))
  for (v in vars) {
    if (force || v %in% .scf_dollar_vars || endsWith(v, "_real")) next
    if (.scf_is_raw(v)) {
      stop(sprintf(paste0(
        "`%s` is a raw SCF variable, which is already in nominal dollars. ",
        "Use force = TRUE to convert it anyway."), v), call. = FALSE)
    }
    stop(sprintf(paste0(
      "`%s` is not one of the Fed's summary dollar variables, such as income ",
      "or networth, or a variable made by scf_real(), so it may not be in ",
      "2022 dollars. Use force = TRUE if it is."), v), call. = FALSE)
  }

  .scf_deflation_factor(x$aux$year, from_year)
}

.scf_nominal_estimate <- function(x, from_year, force, rerun_function) {
  f <- .scf_nominal_setup(x, from_year, force, rerun_function)

  for (col in c("estimate", "se", "min", "max")) {
    if (col %in% names(x$results)) {
      x$results[[col]] <- x$results[[col]] * f
    }
  }

  if (!is.null(x$imps)) {
    x$imps <- lapply(x$imps, function(df) {
      for (col in c("estimate", "se")) {
        if (col %in% names(df)) {
          df[[col]] <- df[[col]] * f
        }
      }
      df
    })
  }

  attr(x, "nominal") <- TRUE
  attr(x, "from_year") <- as.integer(from_year)
  x
}
