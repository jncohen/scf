# tests/testthat/test-scf_plot_dist.R

test_that("scf_plot_dist keeps continuous bins in numeric order", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  p <- scf_plot_dist(scf2022, ~income)
  lower <- as.numeric(sub("-.*$", "", levels(p$data$xval)))

  expect_false(is.unsorted(lower))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_plot_dist treats character variables as categories", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  s <- scf_update(scf2022, e = as.character(edcl))
  p <- scf_plot_dist(s, ~e)
  expect_equal(nrow(p$data), 4)

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
