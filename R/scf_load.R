#' Load SCF Data as Multiply-Imputed Survey Designs
#'
#' @description
#' Converts SCF `.rds` files prepared by [scf_download()] into `scf_mi_survey`
#' objects. Each object wraps five implicates per year in replicate-weighted,
#' multiply-imputed survey designs suitable for use with `scf_*` functions for
#' analysis, plotting, testing, and modeling.
#'
#' @section Implementation:
#' Specify a year or range of SCF years and provide a directory containing
#' the `.rds` files created by [scf_download()]. Each file should contain five
#' implicate-level data frames. These are converted into replicate-weighted survey
#' designs using [survey::svrepdesign()] and wrapped into an `scf_mi_survey` object
#' using [scf_design()]. Only the survey designs (`mi_design`) are retained to ensure
#' efficiency and prevent duplication of large data structures.
#'
#' @section Storage:
#' To conserve memory and promote efficient operations, `scf_mi_survey` objects
#' do not retain raw implicates or pooled data by default. If needed, users can
#' extract implicate-level data from the survey designs using a helper function
#' such as `scf_extract_implicates()` (to be provided).
#'
#' @param min_year Integer. First SCF year to load (must be in 1989–2022 and divisible by 3).
#' @param max_year Integer. Last SCF year to load. Defaults to `min_year`.
#' @param data_directory Character. Path to directory containing `.rds` files. Default is the current working directory.
#'
#' @return Invisibly returns a `scf_mi_survey` object (or a named list of them if multiple years are loaded).
#' The object contains:
#' - `mi_design`: A list of five `svyrep.design` replicate-weighted survey objects (one per implicate).
#' 
#' Attributes may include:
#' - `mock`: Logical flag indicating if the data is a small-set for functional testing and not for analytical results.
#' - `year`: SCF survey year.
#' - `n_households`: Population estimate for that year.
#' 
#' @seealso [scf_download()], [scf_design()], [scf_update()], [scf_subset()], [survey::svrepdesign()]
#'
#' @examples
#' \dontrun{
#' # Download SCF data and load it into a replicate-weighted MI survey object
#' scf_download(2022)
#' scf2022 <- scf_load(2022)
#' }
#'
#' @export
scf_load <- function(min_year,
                     max_year = min_year,
                     data_directory = ".") {

  if (!requireNamespace("survey", quietly = TRUE)) stop("Package 'survey' is required.")

  household_totals <- c(
    `1989` = 92830000, `1992` = 95670000, `1995` = 98990000,
    `1998` = 102530000, `2001` = 108210000, `2004` = 112000000,
    `2007` = 116100000, `2010` = 117540000, `2013` = 122460000,
    `2016` = 125820000, `2019` = 128580000, `2022` = 131202000
  )

  years <- intersect(seq(min_year, max_year, by = 3), names(household_totals))
  if (length(years) == 0) stop("No valid SCF years selected.")
  
  results <- list()
  
  for (year in years) {
    file_path <- file.path(data_directory, paste0("scf", year, ".rds"))
    if (!file.exists(file_path)) {
      warning("File not found: ", file_path)
      next
    }
    
    imp_list <- readRDS(file_path)
    
    if (!is.list(imp_list) || length(imp_list) != 5) {
      warning("File does not contain 5 implicates: ", file_path)
      next
    }
    
    target_total <- household_totals[as.character(year)]
    
    imp_list <- lapply(imp_list, function(df) {
      rep_cols <- grep("^wt1b", names(df), value = TRUE)
      df[rep_cols] <- lapply(df[rep_cols], as.numeric)
      df
    })
    
    rep_cols <- grep("^wt1b", names(imp_list[[1]]), value = TRUE)
    
    imp_designs <- lapply(imp_list, function(df) {
      survey::svrepdesign(
        weights = ~wgt,
        repweights = as.matrix(df[, rep_cols]),
        data = df,
        type = "other",
        scale = 1,
        rscales = rep(1 / 998, 999),
        mse = TRUE,
        combined.weights = TRUE
      )
    })
    
    mi_obj <- scf_design(
      design = imp_designs,
      year = as.integer(year),
      n_households = target_total
    )
    
    attr(mi_obj, "mock") <- FALSE
    
    results[[as.character(year)]] <- mi_obj
  }
  
  if (length(results) == 0L) stop("No valid SCF files loaded.")
  if (length(results) == 1L) {
    invisible(results[[1]])
  } else {
    invisible(results)
  }
}
