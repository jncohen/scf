# tests/testthat/test-scf_plot_hist.R

test_that("scf_plot_hist keeps empty bins on the axis", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  p <- scf_plot_hist(scf2022, ~networth, bins = 30)
  b <- ggplot2::ggplot_build(p)
  expect_equal(length(b$layout$panel_params[[1]]$x$get_labels()), 30)

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
