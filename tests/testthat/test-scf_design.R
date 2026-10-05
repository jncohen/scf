# tests/testthat/test-scf_design.R

test_that("scf_design builds an scf_mi_survey and checks its input", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  obj <- scf_design(scf2022$mi_design, year = 2022, n_households = scf2022$n_households)
  expect_s3_class(obj, "scf_mi_survey")
  expect_length(obj$mi_design, 5)
  expect_equal(obj$year, 2022)
  expect_true(any(grepl("Households (N): ", capture.output(print(obj)), fixed = TRUE)))
  expect_error(scf_design(list(1, 2), 2022, 1), "svyrep.design")

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
