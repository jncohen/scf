#' Load SCF Data as Multiply-Imputed Survey Designs
#'
#' @description
#' Converts SCF `.rds` files prepared by `scf_download()` into `scf_mi_survey`
#' objects. Each object wraps five implicates per year in replicate-weighted,
#' multiply-imputed survey designs suitable for use with `scf_` functions.
#'
#' @section Implementation:
#' Provide a year or range and either (1) a directory containing `scf<year>.rds`
#' files, or (2) a full path to a single `.rds` file. Files must contain five
#' implicate data frames with columns `wgt` and `wt1b1..wt1bK` (typically K=999).
#'
#' @param min_year Integer. First SCF year to load (1989–2022, divisible by 3).
#' @param max_year Integer. Last SCF year to load. Defaults to `min_year`.
#' @param data_directory Character. Directory containing `.rds` files or a 
#'   full path to a single `.rds` file. Defaults to the current working directory `"."`.
#'   For examples and tests, use `tempdir()` to avoid leaving files behind.
#'
#' @return Invisibly returns a `scf_mi_survey` (or named list if multiple years).
#' Attributes: `mock` (logical), `year`, `n_households`.
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
      n_households = sum(imp_list[[1]]$wgt)
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

