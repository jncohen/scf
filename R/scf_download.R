#' Download and Prepare SCF Microdata for Local Analysis
#'
#' Downloads SCF public-use data from the Federal Reserve and saves one `.rds`
#' file per year for [scf_load()].
#'
#' @details
#' Run it once per survey year, before [scf_load()]. Requires an internet
#' connection. Files go to the working directory; years already on disk are
#' skipped unless `overwrite = TRUE`.
#'
#' For each year, the function downloads the full public data, the summary
#' extract, and the replicate weights; merges them by household; multiplies
#' each replicate weight by its multiplicity factor; and saves the five
#' implicates as a list.
#'
#' @param years Integer vector of SCF years to download (e.g., `c(2016, 2019)`). Must be triennial from 1989 to 2022.
#' @param overwrite Logical. If `TRUE`, re-download and overwrite existing `.rds` files. Default is `FALSE`.
#' @param verbose Logical. If `TRUE`, display progress messages. Default is `TRUE`.
#'
#' @return A character vector of paths to the `.rds` files written to disk (one per year).
#' Each file contains a list of five implicate data frames ready for use with [scf_load()].
#'
#' @seealso [scf_load()], [scf_design()], [scf_update()]
#'
#' @examples
#' if (FALSE) {
#'   # Download and prepare SCF data for 2022
#'   td <- tempfile("download_")
#'   dir.create(td)
#'
#'   old <- getwd()
#'   setwd(td)
#'   scf_download(2022)
#'
#'   # Load into a survey design object
#'   scf2022 <- scf_load(2022, data_directory = td)
#'
#'   # Cleanup for package check
#'   unlink(td, recursive = TRUE, force = TRUE)
#'   setwd(old)
#' }
#'
#' @references
#' U.S. Federal Reserve. Codebook for 2022 Survey of Consumer Finances.
#'   https://www.federalreserve.gov/econres/scfindex.htm
#'
#' @export
scf_download <- function(years = seq(1989, 2022, 3), overwrite = FALSE, verbose = TRUE) {
  years <- intersect(years, seq(1989, 2022, 3))
  output_files <- character()

  url_map <- list(
    `1989` = list(
      main = "https://www.federalreserve.gov/econresdata/scf/files/scfp1989s.zip",
      summary = "https://www.federalreserve.gov/econresdata/scf/files/scf89s.zip",
      rw = "https://www.federalreserve.gov/econresdata/scf/files/scf89rw1s.zip"
    ),
    `1992` = list(
      main = "https://www.federalreserve.gov/econresdata/scf/files/scfp1992s.zip",
      summary = "https://www.federalreserve.gov/econresdata/scf/files/scf92s.zip",
      rw = "https://www.federalreserve.gov/econresdata/scf/files/scf92rw1s.zip"
    ),
    `1995` = list(
      main = "https://www.federalreserve.gov/econresdata/scf/files/scfp1995s.zip",
      summary = "https://www.federalreserve.gov/econresdata/scf/files/scf95s.zip",
      rw = "https://www.federalreserve.gov/econresdata/scf/files/scf95rw1s.zip"
    ),
    `1998` = list(
      main = "https://www.federalreserve.gov/econresdata/scf/files/scfp1998s.zip",
      summary = "https://www.federalreserve.gov/econresdata/scf/files/scf98s.zip",
      rw = "https://www.federalreserve.gov/econresdata/scf/files/scf98rw1s.zip"
    ),
    `2001` = list(
      main = "https://www.federalreserve.gov/econres/files/scf01s.zip",
      summary = "https://www.federalreserve.gov/econres/files/scfp2001s.zip",
      rw = "https://www.federalreserve.gov/econres/files/scf2001rw1s.zip"
    )
  )

  for (year in years) {
    if (verbose) message("Processing SCF ", year)
    out_file <- paste0("scf", year, ".rds")
    if (file.exists(out_file) && !overwrite) {
      if (verbose) message("File exists, skipping: ", out_file)
      output_files <- c(output_files, out_file)
      next
    }

    tmpdir <- tempdir()
    year_short <- sprintf("%02d", year %% 100)

    if (year <= 2001) {
      urls <- url_map[[as.character(year)]]
      zip_main <- basename(urls$main)
      zip_summary <- basename(urls$summary)
      zip_rw <- basename(urls$rw)
    } else {
      zip_main <- paste0("scfp", year, "s.zip")
      zip_summary <- paste0("scf", year, "s.zip")
      zip_rw <- paste0("scf", year, "rw1s.zip")
      urls <- list(
        main = paste0("https://www.federalreserve.gov/econres/files/", zip_main),
        summary = paste0("https://www.federalreserve.gov/econres/files/", zip_summary),
        rw = paste0("https://www.federalreserve.gov/econres/files/", zip_rw)
      )
    }

    download_and_unzip <- function(url, zip_name) {
      zip_path <- file.path(tmpdir, zip_name)
      tryCatch({
        resp <- httr::GET(url, httr::write_disk(zip_path, overwrite = TRUE))
        httr::stop_for_status(resp)
        unzip(zip_path, exdir = tmpdir)
        unlink(zip_path)
        TRUE
      }, error = function(e) {
        stop("Download failed: ", url, "\n", conditionMessage(e))
      })
    }

    download_and_unzip(urls$main, zip_main)
    download_and_unzip(urls$summary, zip_summary)
    download_and_unzip(urls$rw, zip_rw)

    main_file <- file.path(tmpdir, paste0("rscfp", year, ".dta"))
    summary_file <- switch(as.character(year),
                           `1989` = "p89i6.dta", `1992` = "p92i4.dta", `1995` = "p95i6.dta",
                           `1998` = "p98i6.dta", `2001` = "p01i6.dta",
                           paste0("p", year_short, "i6.dta")
    )
    summary_file <- file.path(tmpdir, summary_file)

    rw_file <- if (year == 2001) "scf2001rw1s.dta" else paste0("p", year_short, "_rw1.dta")
    rw_file <- file.path(tmpdir, rw_file)

    if (!file.exists(main_file) || !file.exists(summary_file) || !file.exists(rw_file)) {
      stop("Missing one or more required .dta files for ", year)
    }

    main <- haven::read_dta(main_file)
    summary <- haven::read_dta(summary_file)
    rw <- haven::read_dta(rw_file)

    names(main) <- tolower(names(main))
    names(summary) <- tolower(names(summary))
    names(rw) <- tolower(names(rw))

    if (year == 1989) {
      names(main)[names(main) == "x1"] <- "y1"
      names(main)[names(main) == "xx1"] <- "yy1"
      names(summary)[names(summary) == "x1"] <- "y1"
      names(summary)[names(summary) == "xx1"] <- "yy1"
      names(rw)[names(rw) == "x1"] <- "y1"
      names(rw)[names(rw) == "xx1"] <- "yy1"
    }

    if ("yy1" %in% names(summary) && !"yy1" %in% names(main)) {
      main$yy1 <- summary$yy1[match(main$y1, summary$y1)]
    }

    summary <- summary[, setdiff(names(summary), "yy1")]
    data <- merge(main, summary, by = "y1")
    if (!"yy1" %in% names(data)) stop("`yy1` missing after merge")
    data$imp <- as.numeric(substr(data$y1, nchar(data$y1), nchar(data$y1)))
    implicates <- lapply(1:5, function(i) subset(data, imp == i))
    if (any(sapply(implicates, nrow) == 0)) stop("Empty implicate detected for year ", year)

    rw[is.na(rw)] <- 0
    rw$y1 <- NULL
    for (i in 1:999) {
      w <- paste0("wt1b", i)
      m <- paste0("mm", i)
      if (all(c(w, m) %in% names(rw))) {
        rw[[w]] <- rw[[w]] * rw[[m]]
      }
    }
    rw <- rw[, c("yy1", paste0("wt1b", 1:999))]
    merged <- lapply(implicates, function(df) merge(df, rw, by = "yy1"))
    saveRDS(merged, out_file)
    output_files <- c(output_files, out_file)

    unlink(c(main_file, summary_file, rw_file), force = TRUE)
    if (verbose) message("Saved: ", out_file)
  }

  return(output_files)
}
