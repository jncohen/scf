#' Create or Alter SCF Variables
#'
#' Creates or changes variables in every implicate.
#'
#' @details
#' Use it before analysis to recode values, convert types, or create ratios,
#' differences, and indicators. Expressions are evaluated in order in each
#' implicate's data. Set a variable to `NULL` to remove it. The result is a
#' new object; assign it back to keep the changes. For variables that depend
#' on the distribution within each implicate, such as ranks or percentile
#' cutoffs, use [scf_update_by_implicate()].
#'
#' @param .object A `scf_mi_survey` object from [scf_load()].
#' @param ... Named expressions assigning new or modified variables using `=` syntax.
#'   Each must return one value or one value per row. Set a variable to `NULL`
#'   to remove it.
#'
#' @return The input `scf_mi_survey` object with `mi_design` updated to reflect
#' the new or modified variables. Other elements (`year`, `n_households`,
#' `dollar_year`) and the `mock` attribute are kept. `n_households` is not
#' recomputed.
#'
#' @examples
#' \dontshow{
#' td <- tempfile()
#' dir.create(td)
#' file.copy(system.file("extdata", "scf2022_mock_raw.rds", package = "scf"),
#'           file.path(td, "scf2022.rds"))
#' scf2022 <- scf_load(2022, data_directory = td)
#' }
#' # Create a binary indicator for being over age 50
#' scf2022 <- scf_update(scf2022,
#'   over50 = age > 50
#' )
#'
#' # Example: Create a log-transformed income variable
#' scf2022 <- scf_update(scf2022,
#'   log_income = log(income + 1)
#' )
#' \dontshow{unlink(td, recursive = TRUE, force = TRUE)}
#'
#' @seealso [scf_load()], [scf_update_by_implicate()], [survey::svrepdesign()]
#'
#' @export
scf_update <- function(.object, ...) {
  if (!inherits(.object, "scf_mi_survey")) {
    stop("Input must be of class 'scf_mi_survey'.")
  }

  exprs <- as.list(substitute(list(...)))[-1]
  exprs <- exprs[!vapply(exprs, function(e) identical(e, quote(expr = )), logical(1))]
  env <- parent.frame()

  if (is.null(.object$mi_design) || length(.object$mi_design) == 0)
    stop("'mi_design' is empty. Cannot apply update.")

  if (length(exprs) > 0 && (is.null(names(exprs)) || any(names(exprs) == ""))) {
    stop("All arguments must be named, e.g. `over50 = age > 50`.", call. = FALSE)
  }

  updated_designs <- vector("list", length(.object$mi_design))
  names(updated_designs) <- names(.object$mi_design)

  for (i in seq_along(.object$mi_design)) {
    df <- .object$mi_design[[i]]$variables

    for (j in seq_along(exprs)) {
      varname <- names(exprs)[j]
      value <- tryCatch(
        eval(exprs[[j]], df, env),
        error = function(e) {
          stop(sprintf("Could not create `%s` in implicate %d: %s",
                       varname, i, conditionMessage(e)), call. = FALSE)
        }
      )
      if (!is.null(value) && !length(value) %in% c(1L, nrow(df))) {
        stop(sprintf("`%s` has length %d; it must have length 1 or %d.",
                     varname, length(value), nrow(df)), call. = FALSE)
      }
      df[[varname]] <- value
    }

    updated_designs[[i]] <- .scf_svrepdesign(df)
  }

  .object$mi_design <- updated_designs

  if (is.null(attr(.object, "mock"))) {
    attr(.object, "mock") <- FALSE
  }

  return(.object)
}