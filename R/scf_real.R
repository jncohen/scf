#' Convert Raw SCF Dollar Variables to 2022 Dollars
#'
#' Adds 2022-dollar versions of raw `x` dollar variables to every implicate.
#'
#' @details
#' Follows the Fed's SCF Bulletin SAS macro. Amounts are multiplied by the
#' September CPI-U-RS for 2022 divided by that for the survey year. Income
#' items, reported for the prior year, are first multiplied by the ratio of
#' annual CPI-U-RS in the survey year to the prior year; name them in
#' `income`. In other amounts, the codes -1 and -2 are left unchanged. A
#' converted variable equals the matching summary variable; for example,
#' `x5702` with `income = ~x5702` equals `wageinc`.
#'
#' The function stops, unless `force = TRUE`, if a variable is not a raw `x`
#' variable, has fewer than 25 distinct values, or the new variable already
#' exists. It cannot tell which variables are income items; check the SCF
#' codebook.
#'
#' @param scf A `scf_mi_survey` object created by [scf_load()].
#' @param vars A one-sided formula naming raw dollar variables, such as
#'   `~x5729 + x3915`.
#' @param income Optional one-sided formula naming the variables in `vars`
#'   that are income items, reported for the year before the survey.
#' @param suffix Text added to each converted variable's name. Defaults to
#'   `"_real"`.
#' @param force Logical. If `TRUE`, skip the safeguards.
#'
#' @return The `scf_mi_survey` object with the converted variables added.
#'   Assign it back to keep them.
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("real_")
#' dir.create(td)
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: wages (an income item) in 2022 dollars
#' \dontrun{
#' scf2016 <- scf_load(2016)
#' scf2016 <- scf_real(scf2016, ~x5702, income = ~x5702)
#' scf_median(scf2016, ~x5702_real)
#' }
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @seealso [scf_nominal()], [scf_update()]
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
scf_real <- function(scf, vars, income = NULL, suffix = "_real", force = FALSE) {
  if (!inherits(scf, "scf_mi_survey")) {
    stop("Input must be an 'scf_mi_survey' object.", call. = FALSE)
  }
  if (!inherits(vars, "formula")) {
    stop("`vars` must be a one-sided formula, e.g. ~x5729.", call. = FALSE)
  }
  if (!is.null(income) && !inherits(income, "formula")) {
    stop("`income` must be a one-sided formula, e.g. ~x5729.", call. = FALSE)
  }

  .scf_check_year(scf$year)
  var_names <- all.vars(vars)
  income_names <- if (is.null(income)) character(0) else all.vars(income)

  extra <- setdiff(income_names, var_names)
  if (length(extra) > 0) {
    stop(sprintf("`income` names variables not in `vars`: %s.",
                 paste(extra, collapse = ", ")), call. = FALSE)
  }

  first <- scf$mi_design[[1]]$variables
  for (v in var_names) {
    if (!v %in% names(first)) {
      stop(sprintf("`%s` is not in the data.", v), call. = FALSE)
    }
    if (!is.numeric(first[[v]])) {
      stop(sprintf("`%s` is not numeric.", v), call. = FALSE)
    }
    if (force) next
    if (!.scf_is_raw(v)) {
      stop(sprintf(paste0(
        "`%s` is not a raw x variable. Summary variables such as income and ",
        "networth are already in 2022 dollars. Use force = TRUE to convert it anyway."), v),
        call. = FALSE)
    }
    n_values <- length(unique(first[[v]]))
    if (n_values < 25) {
      stop(sprintf(paste0(
        "`%s` has only %d distinct values, so it looks like a code, not a dollar ",
        "amount. Use force = TRUE if it is a dollar amount."), v, n_values),
        call. = FALSE)
    }
    if (paste0(v, suffix) %in% names(first)) {
      stop(sprintf("`%s` already exists. Use force = TRUE to replace it.",
                   paste0(v, suffix)), call. = FALSE)
    }
  }

  year <- as.character(scf$year)
  factor_balance <- .scf_cpi_urs[["2022"]] / .scf_cpi_urs[[year]]
  factor_income <- factor_balance * .scf_cpi_lag[[year]]

  scf_update_by_implicate(scf, function(df) {
    for (v in var_names) {
      value <- df[[v]]
      if (v %in% income_names) {
        value <- value * factor_income
      } else {
        code <- !is.na(value) & value %in% c(-1, -2)
        value[!code] <- value[!code] * factor_balance
      }
      df[[paste0(v, suffix)]] <- value
    }
    df
  })
}
