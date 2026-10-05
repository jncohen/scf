#' Load SCF Data as Multiply-Imputed Survey Designs
#'
#' Loads `.rds` files made by [scf_download()] as `scf_mi_survey` objects.
#'
#' @details
#' Start every analysis here and assign the result, for example
#' `scf2022 <- scf_load(2022)`. Printing the object shows the year, the number
#' of households represented, and the number of implicates.
#'
#' Each file holds five implicate data frames with the weight `wgt` and
#' replicate weights `wt1b1` to `wt1b999`. Each implicate's design uses
#' `wgt * 5`, because the Fed divides `wgt` by 5 so the five implicates sum to
#' the population. The `wgt` column is not changed.
#'
#' @param min_year Integer. First SCF year to load (1989 to 2022).
#' @param max_year Integer. Last SCF year to load. Defaults to `min_year`.
#' @param data_directory Character. Directory containing the `.rds` files.
#'   Defaults to the current working directory `"."`.
#'   For examples and tests, use `tempdir()` to avoid leaving files behind.
#'
#' @return Invisibly, a `scf_mi_survey` object (or a named list of them if several years
#' are loaded), with elements `mi_design`, `year`, and `n_households`, and a
#' logical `mock` attribute.
#'
#' @seealso [scf_download()], [scf_design()], [scf_update()], [survey::svrepdesign()]
#'
#' @examples
#' # Using with CRAN-compliant mock data:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("load_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @export
scf_load <- function(min_year,
                     max_year = min_year,
                     data_directory = ".") {

  valid_years <- seq(1989, 2022, by = 3)

  years <- valid_years[valid_years >= min_year & valid_years <= max_year]
  if (length(years) == 0) stop("No valid SCF years selected.")

  results <- list()

  for (year in years) {
    file_path <- file.path(data_directory, paste0("scf", year, ".rds"))
    if (!file.exists(file_path)) {
      warning("File not found: ", file_path)
      next
    }

    imp_list <- readRDS(file_path)
    is_mock <- isTRUE(attr(imp_list, "mock"))

    if (!is.list(imp_list) || length(imp_list) != 5) {
      warning("File does not contain 5 implicates: ", file_path)
      next
    }

    imp_list <- lapply(imp_list, function(df) {
      rep_cols <- grep("^wt1b", names(df), value = TRUE)
      df[rep_cols] <- lapply(df[rep_cols], as.numeric)
      df
    })

    imp_designs <- lapply(imp_list, .scf_svrepdesign)

    mi_obj <- scf_design(
      design = imp_designs,
      year = as.integer(year),
      n_households = sum(imp_list[[1]]$wgt) * 5
    )

    attr(mi_obj, "mock") <- is_mock
    if (is_mock && !isTRUE(.scf_state$mock_noted)) {
      message("Loaded mock SCF data. Use it for examples and tests only, not for analysis.")
      .scf_state$mock_noted <- TRUE
    }

    results[[as.character(year)]] <- mi_obj
  }

  if (length(results) == 0L) stop("No valid SCF files loaded.")
  if (length(results) == 1L) {
    invisible(results[[1]])
  } else {
    invisible(results)
  }
}

