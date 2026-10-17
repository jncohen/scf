#' Modify Each Implicate Individually in SCF Data
#'
#' Applies a function to each implicate's data frame.
#'
#' @details
#' Use for variables that depend on the distribution within each implicate,
#' such as ranks, implicate-specific quantiles, or group z-scores. The
#' function must return a data frame with the same number of rows. The result
#' is a new object; assign it back to keep the changes.
#'
#' @param object A `scf_mi_survey` object from [scf_load()].
#' @param f A function that takes one implicate's data frame as its sole argument
#'   and returns a modified data frame with the same number of rows. The function
#'   signature should be \code{function(df) \{ ... \}}. The returned data frame is
#'   used to rebuild the replicate-weighted survey design.
#'
#'
#' @return A modified `scf_mi_survey` object with updated implicate-level designs.
#'
#' @examples
#' \dontshow{
#' td <- tempfile()
#' dir.create(td)
#' file.copy(system.file("extdata", "scf2022_mock_raw.rds", package = "scf"),
#'           file.path(td, "scf2022.rds"))
#' scf2022 <- scf_load(2022, data_directory = td)
#' }
#' # Flag households in the top 10% of net worth,
#' # using each implicate's weighted 90th percentile as the threshold.
#' scf2022 <- scf_update_by_implicate(scf2022, function(df) {
#'   o <- order(df$networth)
#'   share <- cumsum(df$wgt[o]) / sum(df$wgt)
#'   threshold <- df$networth[o][which(share >= 0.90)[1]]
#'   df$top10nw <- df$networth >= threshold
#'   df
#' })
#'
#' # Compute implicate-specific z-scores of income,
#' # using the weighted mean and standard deviation
#' scf2022 <- scf_update_by_implicate(scf2022, function(df) {
#'   mu <- stats::weighted.mean(df$income, df$wgt)
#'   sigma <- sqrt(stats::weighted.mean((df$income - mu)^2, df$wgt))
#'   df$z_income <- (df$income - mu) / sigma
#'   df
#' })
#'
#' # Verify new variable exists
#' head(scf2022$mi_design[[1]]$variables$z_income)
#' \dontshow{unlink(td, recursive = TRUE, force = TRUE)}
#'
#' @seealso [scf_update()]
#' @export
scf_update_by_implicate <- function(object, f) {
  if (!inherits(object, "scf_mi_survey")) {
    stop("Input must be a 'scf_mi_survey' object.")
  }
  if (!is.function(f)) {
    stop("Argument 'f' must be a function that accepts a data frame and returns a data frame.")
  }

  updated_designs <- vector("list", length(object$mi_design))

  for (i in seq_along(object$mi_design)) {
    design <- object$mi_design[[i]]
    df <- design$variables
    new_df <- f(df)

    if (!is.data.frame(new_df)) {
      stop(sprintf("Function `f` must return a data.frame. Implicate %d returned: %s",
                   i, class(new_df)))
    }
    if (nrow(new_df) != nrow(df)) {
      stop(sprintf("Row count mismatch in implicate %d: original = %d, new = %d",
                   i, nrow(df), nrow(new_df)))
    }

    updated_designs[[i]] <- .scf_svrepdesign(new_df)
  }

  object$mi_design <- updated_designs
  return(object)
}
