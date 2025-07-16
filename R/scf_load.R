#' Load SCF Data as Multiply-Imputed Survey Designs.
#'
#' @description
#' Converts SCF `.rds` files prepared by [scf_download()] into `scf_mi_survey`
#' objects. Each object wraps five implicates per year in replicate-weighted,
#' multiply-imputed survey designs suitable for use with `scf_*` functions for
#' analysis, plotting, testing, and modeling.
#'
#' @section Implementation:
#' Specify a year or range of SCF years and provide a directory containing
#' the `.rds` files created by [scf_download()]. Files are loaded into a list
#' of five implicate-level data frames, each converted into a replicate-weighted
#' survey object using [survey::svrepdesign()]. The final object is constructed
#' using [scf_design()].
#'
#' @section Memory Usage:
#' By default, the returned object includes only the list of survey designs
#' (`mi_design`). To reduce memory footprint, `light = TRUE` excludes the raw
#' implicate data (`implicates`) and long-format pooled data (`data`). Set
#' `light = FALSE` to include these optional components.
#'
#' @param min_year Integer. First SCF year to load (must be in 1989–2022 and divisible by 3).
#' @param max_year Integer. Last SCF year to load. Defaults to `min_year`.
#' @param data_directory Character. Path to directory containing `.rds` files. Default is the current working directory.
#' @param light Logical. If `TRUE` (default), return a memory-efficient object containing only the survey designs. If `FALSE`, also include raw implicates and pooled long-format data.
#'
#' @return Invisibly returns a `scf_mi_survey` object (or a named list of them if multiple years are loaded).
#' Assign the result to a variable (e.g., `scf2022 <- scf_load(2022)`) to access the object.
#' - `mi_design`: A list of five `svyrep.design` replicate-weighted survey objects.
#' - `implicates`: *(Optional)* A list of raw implicate data frames.
#' - `data`: *(Optional)* A pooled long-format data frame combining all five implicates, with an `implicate` column.
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
                     data_directory = ".",
                     light = TRUE) {

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

    pooled <- NULL
    if (!light) {
      pooled <- dplyr::bind_rows(lapply(seq_along(imp_list), function(i) {
        d <- imp_list[[i]]
        d$implicate <- i
        d
      }))
    }

    if (light) {
      imp_list <- NULL
    }

    mi_obj <- scf_design(
      design = imp_designs,
      implicates = imp_list,
      year = as.integer(year),
      n_households = target_total
    )
    if (!light) {
      mi_obj$data <- pooled
    }

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
