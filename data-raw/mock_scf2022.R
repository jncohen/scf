# Generate a mock SCF 2022 dataset for CRAN-safe use
# data-raw/mock_scf2022.R

library(scf)
library(dplyr)
library(survey)

set.seed(123)

# Step 1: Load full data with implicates
full <- scf_load(2022, light = FALSE)
imps <- full$implicates

# Step 2: Select 200 common households using yy1 from implicate 1
rows <- imps[[1]]$yy1[1:200]

# Step 3: Filter same 200 from all implicates
reduced_imps <- lapply(imps, \(df) df[df$yy1 %in% rows, ])

# Step 4: Copy replicate weights from implicate 1
rep_weights <- reduced_imps[[1]] |>
  select(yy1, starts_with("wt1b"))

reduced_imps[2:5] <- lapply(reduced_imps[2:5], \(df) {
  left_join(df %>% select(-starts_with("wt1b")), rep_weights, by = "yy1")
})

# Step 5: Build svrep.design objects
rep_cols <- grep("^wt1b", names(reduced_imps[[1]]), value = TRUE)
mock_designs <- lapply(reduced_imps, \(df) {
  svrepdesign(
    weights = ~wgt,
    repweights = df[, rep_cols],
    data = df,
    type = "other",
    scale = 1,
    rscales = rep(1 / 998, 999),
    mse = TRUE,
    combined.weights = TRUE
  )
})

# Step 6: Build mock object (light version)
mock <- scf_design(
  design = mock_designs,
  implicates = reduced_imps,
  year = 2022,
  n_households = 131202000
)
attr(mock, "mock") <- TRUE


# Step 7: Save to inst/extdata
saveRDS(mock, file = "inst/extdata/mock_scf2022.rds")
