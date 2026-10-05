# tests/testthat/test-scf_ratio.R

test_that("scf_ratio matches survey and mitools", {
  skip_if_not_installed("mitools")

  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)
  scf2022 <- scf_update(scf2022, owner_income = income * (own == 1))

  r <- scf_ratio(scf2022, ~owner_income, ~income, variance = "rubin")
  ref <- mitools::MIcombine(lapply(scf2022$mi_design, function(d) {
    survey::svyratio(~owner_income, ~income, d)
  }))
  expect_equal(r$results$estimate, unname(coef(ref)))
  expect_equal(r$results$se, unname(survey::SE(ref)))

  per <- lapply(scf2022$mi_design, function(d) survey::svyratio(~owner_income, ~income, d))
  e <- sapply(per, coef)
  v <- sapply(per, function(x) survey::SE(x)^2)
  f <- scf_ratio(scf2022, ~owner_income, ~income)
  expect_equal(f$results$se, unname(sqrt(v[1] + 1.2 * stats::var(e))))

  g <- scf_ratio(scf2022, ~owner_income, ~income, by = ~edcl)
  expect_equal(nrow(g$results), 4)
  expect_no_error(capture.output(print(g)))
  expect_equal(nrow(scf_implicates(g, long = TRUE)), 20)

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
