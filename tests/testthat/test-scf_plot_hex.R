# tests/testthat/test-scf_plot_hex.R

test_that("scf_plot_hex uses one sampling weight per row", {
  skip_if_not_installed("hexbin")

  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  p <- scf_plot_hex(scf2022, ~income, ~networth)
  n <- sum(sapply(scf2022$mi_design, function(d) nrow(d$variables)))
  total <- mean(sapply(scf2022$mi_design, function(d) sum(weights(d, "sampling"))))

  expect_equal(nrow(p$data), n)
  expect_equal(sum(ggplot2::layer_data(p)$count), total)

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
