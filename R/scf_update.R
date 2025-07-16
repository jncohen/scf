#' Create or Alter SCF Variables
#'
#' @description
#' Use this function to create or alter SCF variables once the raw data set has
#' been loaded into memory using the `scf_load()` function. This function
#' updates an `scf_mi_survey` object by evaluating transformations within each
#' implicate, and then returning a new object with the new or amended variables.
#'
#' Most of the time, you can use `scf_update()` to define variables based on
#' simple logical conditions, arithmetic transformations, or categorical
#' binning — these rules are evaluated separately in each implicate, using the
#' same formula. However, if the transformation you want to apply depends on the
#' distribution of the data within each implicate — such as computing an
#' average percentile or ranking households across all implicates —
#' this function will not suffice. In those cases, use
#' [scf_update_by_implicate()] to write a custom function that operates on each
#' implicate individually.
#'
#' @section Usage:
#' Use `scf_update()` during data wrangling to clean, create, or alter variables before calculating
#' statistics or running models. The function is useful when the analyst wishes to:
#' - Recode missing values that are coded as numeric data
#' - Recast variables that are not in the desired format (e.g., converting a numeric variable to a factor)
#' - Create new variables based on existing ones (e.g., calculating ratios, differences, or indicators)
#'
#' @param object A `scf_mi_survey` object, typically created by [scf_load()].
#' @param ... Named expressions assigning new or modified variables using `=` syntax.
#'   Each expression must return a vector of the same length as the implicate data frame.
#'
#' @return A new `scf_mi_survey` object with:
#' \describe{
#'   \item{implicates}{A list of updated data frames (one per implicate).}
#'   \item{mi_design}{A list of updated `svyrep.design` survey objects.}
#'   \item{data}{(If present in the original object) unchanged pooled data.}
#' }
#'
#' @examples
#' \donttest{
#' # Load bundled mock data (for demonstration only — not real SCF data)
#' scf2022 <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))
#'
#' # For real analysis, use:
#' # scf_download(2022); scf2022 <- scf_load(2022)
#'
#' # Create a binary indicator for being over age 50
#' scf2022 <- scf_update(scf2022,
#'   over50 = age > 50
#' )
#'
#' # Create a log-transformed income variable
#' scf2022 <- scf_update(scf2022,
#'   log_income = log(income + 1)
#' )
#' }
#'
#' @seealso [scf_load()], [scf_update_by_implicate()], [survey::svrepdesign()]
#'
#' @export
scf_update <- function(object, ...) {
  if (!inherits(object, "scf_mi_survey")) {
    stop("Input must be of class 'scf_mi_survey'.")
  }

  if (isTRUE(attr(object, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }

  dots <- substitute(list(...))[-1]
  calling_env <- parent.frame()

  updated_implicates <- vector("list", length(object$implicates))
  updated_designs <- vector("list", length(object$implicates))

  for (i in seq_along(object$implicates)) {
    df <- object$implicates[[i]]
    eval_env <- list2env(df, parent = calling_env)

    for (varname in names(dots)) {
      expr <- dots[[varname]]
      value <- try(
        if (is.function(expr)) expr(df) else eval(expr, envir = eval_env),
        silent = TRUE
      )


      if (inherits(value, "try-error")) {
        stop(sprintf("Failed to evaluate expression for '%s': %s", varname, value))
      }
      if (!is.null(value) && length(value) != nrow(df)) {
        stop(sprintf("Length mismatch for '%s': got %d, expected %d", varname, length(value), nrow(df)))
      }

      df[[varname]] <- value
      assign(varname, value, envir = eval_env)

      if (all(is.na(value))) {
        warning(sprintf("Variable '%s' is all NA. Check logic or input references.", varname))
      }
    }

    updated_implicates[[i]] <- df

    rep_cols <- grep("^wt1b", names(df), value = TRUE)
    updated_designs[[i]] <- survey::svrepdesign(
      weights = ~wgt,
      repweights = df[, rep_cols],
      data = df,
      type = "BRR",
      fay.rho = 0.5,
      mse = TRUE,
      combined.weights = TRUE
    )
  }

  object$implicates <- updated_implicates
  object$mi_design <- updated_designs
  return(object)
}
