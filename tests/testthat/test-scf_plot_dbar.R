# tests/testthat/test-scf_plot_dbar.R

test_that("partial label maps keep unmapped categories", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  p <- scf_plot_dbar(scf2022, ~edcl,
                                      label_map = c("1" = "LessHS", "2" = "HS"))
  expect_equal(as.character(p$data$category), c("LessHS", "HS", "3", "4"))

  full <- c("4" = "College", "3" = "Some", "2" = "HS", "1" = "LessHS")
  q <- scf_plot_dbar(scf2022, ~edcl, label_map = full)
  expect_equal(levels(q$data$category), c("College", "Some", "HS", "LessHS"))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
