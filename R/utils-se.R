#' Standard Errors of scf Results
#'
#' Returns the pooled standard errors of a result.
#'
#' @details
#' Works on results from [scf_mean()], [scf_median()], [scf_percentile()],
#' [scf_ratio()], the models ([scf_ols()], [scf_logit()], [scf_glm()],
#' [scf_quantreg()]), and [scf_MIcombine()]. The values are the `se` column of
#' the results, named by group, or by variable when there is no `by`.
#' Stack-method percentiles have no standard error and return `NA`. For
#' [scf_logit()], standard errors are on the log-odds scale, unlike
#' `std.error` in `results`, which is on the odds-ratio scale.
#'
#' @param object A result from one of the functions above.
#' @param ... Not used.
#'
#' @return A named numeric vector of standard errors.
#'
#' @examples
#' \dontshow{
#' td <- tempfile("se_")
#' dir.create(td)
#' file.copy(system.file("extdata", "scf2022_mock_raw.rds", package = "scf"),
#'           file.path(td, "scf2022.rds"))
#' scf2022 <- scf_load(2022, data_directory = td)
#' }
#' SE(scf_mean(scf2022, ~networth, by = ~edcl))
#' \dontshow{unlink(td, recursive = TRUE, force = TRUE)}
#'
#' @seealso [scf_MIcombine()], [scf_variance]
#' @name scf_se
NULL

.scf_named_se <- function(object) {
  r <- object$results
  nm <- if ("group" %in% names(r)) {
    as.character(r$group)
  } else if ("variable" %in% names(r)) {
    as.character(r$variable)
  } else {
    NULL
  }
  stats::setNames(r$se, nm)
}

#' @rdname scf_se
#' @export
SE.scf_mean <- function(object, ...) {
  .scf_named_se(object)
}

#' @rdname scf_se
#' @export
SE.scf_percentile <- function(object, ...) {
  .scf_named_se(object)
}

#' @rdname scf_se
#' @export
SE.scf_ratio <- function(object, ...) {
  r <- object$results
  nm <- if (is.null(object$aux$byname)) {
    paste(r$numerator, "/", r$denominator)
  } else {
    as.character(r$group)
  }
  stats::setNames(r$se, nm)
}

#' @rdname scf_se
#' @export
SE.scf_model_result <- function(object, ...) {
  sqrt(diag(as.matrix(stats::vcov(object))))
}
