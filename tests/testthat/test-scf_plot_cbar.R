# tests/testthat/test-scf_plot_cbar.R

test_that("scf_plot_cbar orders bars by group level", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  p <- scf_plot_cbar(scf2022, ~networth, ~edcl)
  expect_equal(levels(p$data$group), as.character(sort(unique(scf2022$mi_design[[1]]$variables$edcl))))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
