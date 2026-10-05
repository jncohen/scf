# tests/testthat/test-scf_prop_test.R

test_that("scf_prop_test defaults the null to 0.5 one-sample and 0 two-sample", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  scf2022 <- scf_update(scf2022,
                        rich = as.integer(networth > 1e6),
                        female = factor(hhsex == 2, labels = c("Male", "Female")))

  one <- scf_prop_test(scf2022, ~rich)
  two <- scf_prop_test(scf2022, ~rich, ~female)

  expect_equal(one$fit$null.value, 0.5)
  expect_equal(two$fit$null.value, 0)

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_prop_test uses replicate-weight standard errors and Rubin pooling", {
  skip_if_not_installed("mitools")

  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  scf2022 <- scf_update(scf2022,
                        rich = as.integer(networth > 1e6),
                        female = factor(hhsex == 2, labels = c("Male", "Female")))

  one <- scf_prop_test(scf2022, ~rich, p = 0.15, variance = "rubin")
  two <- scf_prop_test(scf2022, ~rich, ~female, variance = "rubin")

  ref_one <- mitools::MIcombine(lapply(scf2022$mi_design, function(d) {
    survey::svymean(~rich, d)
  }))
  ref_two <- mitools::MIcombine(lapply(scf2022$mi_design, function(d) {
    by <- survey::svyby(~rich, ~female, d, survey::svymean, covmat = TRUE)
    survey::svycontrast(by, c(1, -1))
  }))

  expect_equal(one$results$estimate, unname(coef(ref_one)))
  expect_equal(one$results$std.error, unname(survey::SE(ref_one)))
  expect_equal(one$results$df, unname(ref_one$df))
  expect_equal(two$results$estimate, unname(coef(ref_two)))
  expect_equal(two$results$std.error, unname(survey::SE(ref_two)))
  expect_equal(two$results$df, unname(ref_two$df))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
