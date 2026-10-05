# tests/testthat/test-scf_load.R

test_that("scf_load keeps the mock flag", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  expect_true(isTRUE(attr(scf2022, "mock")))
  expect_no_warning(scf_mean(scf2022, ~networth))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_load puts main and replicate weights on the same scale", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  d <- scf2022$mi_design[[1]]
  main_total <- sum(weights(d, "sampling"))
  rep_totals <- colSums(as.matrix(d$repweights))

  expect_equal(main_total, sum(d$variables$wgt) * 5)
  expect_equal(scf2022$n_households, main_total)
  expect_lt(abs(mean(rep_totals) / main_total - 1), 0.05)

  d$variables$one <- 1
  tot <- survey::svytotal(~one, d)
  expect_equal(unname(coef(tot)), main_total)
  expect_lt(unname(survey::SE(tot)), 0.25 * main_total)

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_load accepts a start year that is not a survey year", {
  td <- tempfile()
  dir.create(td)
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)

  s <- scf_load(2020, 2022, data_directory = td)
  expect_equal(s$year, 2022L)

  unlink(td, recursive = TRUE, force = TRUE)
})
