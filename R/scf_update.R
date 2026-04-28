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
#' binning. These rules are evaluated separately in each implicate, using the
#' same formula. However, if the transformation you want to apply depends on the
#' distribution of the data within each implicate, such as computing an
#' average percentile or ranking households across all implicates, 
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
#' @return The input `scf_mi_survey` object with `mi_design` updated to reflect
#' the new or modified variables. All other attributes (`year`, `n_households`,
#' `mock`) are preserved unchanged.
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("update_")
#' dir.create(td)
#' 
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Create a binary indicator for being over age 50
#' scf2022 <- scf_update(scf2022,
#'   over50 = age > 50
#' )
#'
#' # Example: Create a log-transformed income variable
#' scf2022 <- scf_update(scf2022,
#'   log_income = log(income + 1)
#' )
#' 
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @seealso [scf_load()], [scf_update_by_implicate()], [survey::svrepdesign()]
#'
#' @export
scf_update <- function(object, ...) {
  
  if (!inherits(object, "scf_mi_survey")) {
    stop("Input must be of class 'scf_mi_survey'.")
  }
  
  exprs <- rlang::enquos(...)

  if (is.null(object$mi_design) || length(object$mi_design) == 0)
    stop("'mi_design' is empty. Cannot apply update.")

  updated_designs <- vector("list", length(object$mi_design))

  for (i in seq_along(object$mi_design)) {
    df <- object$mi_design[[i]]$variables

    for (j in seq_along(exprs)) {
      varname <- names(exprs)[j]
      try({
        df[[varname]] <- rlang::eval_tidy(exprs[[j]], data = df)
      }, silent = TRUE)
    }

    rep_cols <- grep("^wt1b", names(df), value = TRUE)
    updated_designs[[i]] <- survey::svrepdesign(
      weights          = ~wgt,
      repweights       = as.matrix(df[, rep_cols]),
      data             = df,
      type             = "other",
      scale            = 1,
      rscales          = rep(1 / 998, 999),
      mse              = TRUE,
      combined.weights = TRUE
    )
  }

  object$mi_design <- updated_designs
  
  if (!inherits(object, "scf_mi_survey")) {
    class(object) <- "scf_mi_survey"
  }
  if (is.null(attr(object, "mock"))) {
    attr(object, "mock") <- FALSE
  }
  
  return(object)
}