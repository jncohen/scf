#' Subset an `scf_mi_survey` Object
#'
#' @description
#' Subsetting refers to the process of retaining only those observations that
#' satisfy a logical (TRUE/FALSE) condition. This function applies such a
#' filter independently to each implicate in an `scf_mi_survey` object created
#' by [scf_design()] via [scf_load()]. The result is a new multiply-imputed,
#' replicate-weighted survey object with appropriately restricted designs.
#'
#' @section Implementation:
#' Use `scf_subset()` to focus analysis on analytically meaningful
#' sub-populations. For example, to analyze only households headed by seniors:
#' 
#' \preformatted{
#' scf2022_seniors <- scf_subset(scf2022, age >= 65)
#' }
#'
#' This is especially useful when analyzing populations such as renters, homeowners, specific age brackets,
#' or any group defined by logical expressions over SCF variables.
#'
#' @section Details:
#' Filtering is conducted separately in each implicate. This preserves valid design structure but means
#' that the same household may fall into or out of the subset depending on imputed values.
#' For example, a household with five different age imputations—say, 64, 66, 63, 65, and 67—would be
#' classified as a senior in only three of five implicates if subsetting on `age >= 65`.
#'
#' Empty subsets in any implicate can cause downstream analysis to fail. Always check subgroup sizes after subsetting.
#'
#' @param scf A `scf_mi_survey` object, typically created by [scf_load()].
#' @param expr A logical expression used to filter rows, evaluated separately in each implicate's variable frame (e.g., `age < 65 & own == 1`).
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

  if (isTRUE(attr(scf, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }

  expr <- substitute(expr)

  new_designs <- vector("list", length(scf$mi_design))
  names(new_designs) <- names(scf$mi_design)

  for (i in seq_along(scf$mi_design)) {
    d <- scf$mi_design[[i]]
    dsub <- subset(d, eval(expr, d$variables, parent.frame()))
    new_designs[[i]] <- dsub
  }

  new_data <- do.call(rbind, lapply(seq_along(new_designs), function(i) {
    df <- new_designs[[i]]$variables
    df$implicate <- i
    df
  }))

  new_implicates <- if (!is.null(scf$implicates)) {
    lapply(scf$implicates, function(df) df[eval(expr, df, parent.frame()), ])
  } else {
    NULL
  }

  result <- structure(
    list(
      mi_design = new_designs,
      data = new_data,
      implicates = new_implicates,
      year = scf$year,
      n_households = sum(new_designs[[1]]$variables$wgt)
    ),
    class = "scf_mi_survey"
  )
  attr(result, "mock") <- isTRUE(attr(scf, "mock"))
  result
}
