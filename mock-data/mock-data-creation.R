# ------------------------------------------------------------------------------
# Mock SCF Data Construction
#
# This script creates a reduced-size "mock" SCF dataset for use in package
# examples, vignettes, and CRAN checks. It is NOT real SCF microdata.
#
# Purpose:
#   - Provide lightweight demonstration data that mimics the structure of
#     official SCF implicates (five implicates, replicate weights).
#   - Ensure package examples run quickly and without requiring external
#     downloads during CRAN checks.
#   - Allow users to experiment with functions without needing the full SCF.
#
# Construction:
#   1. Use scf_download(2022) to retrieve official 2022 SCF microdata.
#   2. Read scf2022.rds: a list of 5 implicates, each a data.frame with
#      replicate weights (wt1b1 ... wt1b999).
#   3. Retain only a small set of variables needed in examples:
#        wgt, wt1b1...wt1b999, age, income, networth, own, hhsex, edcl
#   4. Subset to first 500 rows per implicate (adjustable for CRAN size limits).
#   5. Attach attribute mock = TRUE to mark that this is artificial/demo data.
#   6. Save as scf2022_mock_raw.rds with xz compression (< 5 MB).
#
# Usage:
#   - Distributed in inst/extdata for examples and vignettes.
#   - Loaded via:
#       td  <- tempdir()
#       src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#       file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#       scf2022 <- scf_load(2022, data_directory = td)
#
# Limitations:
#   - This is a toy dataset; estimates are not valid SCF statistics.
#   - Results should NEVER be interpreted as empirical findings.
#   - Intended solely for testing workflows and illustrating function use.
# ------------------------------------------------------------------------------

rm(list = ls()); gc()
library(scf)

setwd("D:/Dropbox/Data/scf/mock-data")
scf_download(2022)                   # produces scf2022.rds in cwd
imp <- readRDS("scf2022.rds")        # list of 5 data.frames

rep_cols <- paste0("wt1b", 1:999)
keep_vars <- c("y1", "Y1", "x1", "X1",
               "wgt", rep_cols, 
               "age","income","networth","own","hhsex","edcl")  

imp_small <- lapply(imp, function(df) {
  df <- df[ , intersect(keep_vars, names(df))]
  df[1:200, , drop = FALSE]          # adjust rows if size > 5MB
})

attr(imp_small, "mock") <- TRUE
saveRDS(imp_small, "scf2022_mock_raw.rds", compress = "xz")

file.copy(
  "D:/Dropbox/Data/scf/mock-data/scf2022_mock_raw.rds",
  "D:/Dropbox/Data/scf/inst/extdata/scf2022_mock_raw.rds",
  overwrite = TRUE
)
