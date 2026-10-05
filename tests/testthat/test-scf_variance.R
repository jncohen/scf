# tests/testthat/test-scf_variance.R

test_that("fed and rubin methods pool sampling variance as documented", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  per <- lapply(scf2022$mi_design, function(d) survey::svymean(~networth, d))
  est <- sapply(per, coef)
  v <- sapply(per, function(x) as.numeric(vcov(x)))
  b <- stats::var(est)

  fed <- scf_mean(scf2022, ~networth, variance = "fed")
  rubin <- scf_mean(scf2022, ~networth, variance = "rubin")

  expect_equal(fed$results$estimate, mean(est))
  expect_equal(fed$results$se, sqrt(v[1] + 1.2 * b))
  expect_equal(rubin$results$se, sqrt(mean(v) + 1.2 * b))

  old <- options(scf.variance = "rubin")
  on.exit(options(old), add = TRUE)
  expect_equal(scf_mean(scf2022, ~networth)$results$se,
               rubin$results$se)
  options(old)
  expect_equal(scf_mean(scf2022, ~networth)$results$se,
               fed$results$se)

  med_fed <- scf_median(scf2022, ~networth, variance = "fed")
  med_rub <- scf_median(scf2022, ~networth, variance = "rubin")
  expect_false(isTRUE(all.equal(med_fed$results$se, med_rub$results$se)))

  expect_error(scf_mean(scf2022, ~networth, variance = "other"))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
