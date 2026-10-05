# tests/testthat/test-scf_plot_bbar.R

test_that("scf_plot_bbar plots shares that match scf_xtab", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  p <- scf_plot_bbar(scf2022, ~edcl, ~own, percent_by = "row")
  x <- scf_xtab(scf2022, ~edcl, ~own)
  expect_s3_class(p, "ggplot")
  expect_equal(sort(p$data$yval), sort(100 * x$results$row_share))
  sums <- tapply(p$data$yval, p$data$row, sum)
  expect_true(all(abs(sums - 100) < 1e-8))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
