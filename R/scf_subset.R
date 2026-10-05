#' Subset an `scf_mi_survey` Object
#'
#' Keeps households that meet a condition, in every implicate.
#'
#' @details
#' Use it to study one part of the population, such as renters or an age
#' range. Estimates from a subset describe that group only: a mean from
#' `scf_subset(scf2022, age >= 65)` is the mean among senior-headed
#' households. To compare groups side by side, use `by` in the estimating
#' functions instead.
#'
#' The condition is applied in each implicate separately, so a household with
#' imputed values can be in the subset in some implicates and not others.
#' Rows where the condition is `NA` are dropped. The function stops if no rows
#' meet the condition in an implicate. `n_households` is the weighted count,
#' averaged across implicates.
#'
#' @param scf A `scf_mi_survey` object, typically created by [scf_load()].
#' @param expr A logical expression used to filter rows, evaluated separately in each implicate's variable frame (e.g., `age < 65 & own == 1`). It can also use variables from the calling environment.
#'
#' @return A new `scf_mi_survey` object (see [scf_design()])
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("subset_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Filter for working-age households with positive net worth
#' scf_sub <- scf_subset(scf2022, age < 65 & networth > 0)
#' scf_mean(scf_sub, ~income)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#'
#' @seealso [scf_load()], [scf_update()]
#'
#' @export
scf_subset <- function(scf, expr) {
  if (!inherits(scf, "scf_mi_survey")) stop("Input must be a `scf_mi_survey` object.")

  expr <- substitute(expr)
  env <- parent.frame()

  new_designs <- vector("list", length(scf$mi_design))
  names(new_designs) <- names(scf$mi_design)

  for (i in seq_along(scf$mi_design)) {
    d <- scf$mi_design[[i]]
    keep <- eval(expr, d$variables, env)
    if (!is.logical(keep) || length(keep) != nrow(d$variables)) {
      stop("`expr` must give one TRUE/FALSE value per row.", call. = FALSE)
    }
    keep[is.na(keep)] <- FALSE
    if (!any(keep)) {
      stop("No rows meet the condition in implicate ", i, ".", call. = FALSE)
    }
    new_designs[[i]] <- d[keep, ]
  }

  result <- structure(
    list(
      mi_design = new_designs,
      year = scf$year,
      n_households = mean(sapply(new_designs, function(d) sum(weights(d, "sampling"))))
    ),
    class = "scf_mi_survey"
  )
  attr(result, "mock") <- isTRUE(attr(scf, "mock"))
  result
}
