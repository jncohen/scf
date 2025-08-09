# Creating Reduced Mock SCF Object for Package Testing
# Joseph Cohen
# 8/8/2025 (updated PM)
#
# This script creates a small, structurally faithful SCF object for testing.
# The resulting object:
# - Uses only the first 75 rows of each implicate
# - Retains all 999 replicate weights per implicate
# - Builds a valid scf_mi_survey object using svrepdesign()
# - Retains only the replicate-weighted designs (light mode = default)
# - Saves the object as "mock_scf2022.rds" for inclusion in /inst/extdata/
#
# IMPORTANT:
# - This object is for package functionality testing only.
# - It is not suitable for analytical use or statistical inference.

rm(list = ls())
gc()

library(scf)
library(survey)

setwd("D:/Dropbox/Data/scf/mock-data")

scf_download(2022)

# Load full implicate set
implicates <- readRDS("scf2022.rds")

# Keep first 75 rows and full 999 replicate weights
rep_cols <- paste0("wt1b", 1:999)
implicates_small <- lapply(implicates, function(df) {
  df <- df[1:75, ]
  keep_vars <- c("wgt", rep_cols, "age", "income", "networth", "own", "hhsex", "x101", "edcl")
  df <- df[ , intersect(keep_vars, names(df))]
  return(df)
})

# Define correct replicate scale vector
rscales_999 <- rep(1 / 998, 999)

# Build replicate-weighted survey designs
designs <- lapply(implicates_small, function(df) {
  svrepdesign(
    weights = ~wgt,
    repweights = as.matrix(df[, rep_cols]),
    data = df,
    type = "other",
    scale = 1,
    rscales = rscales_999,
    mse = TRUE,
    combined.weights = TRUE
  )
})

# Wrap into scf_mi_survey object
scf2022 <- scf_design(
  design = designs,
  year = 2022,
  n_households = 131202000  # Household total for 2022
)

# Flag as mock object
attr(scf2022, "mock") <- TRUE

# Save to mock-data directory
saveRDS(scf2022, "mock_scf2022.rds")
