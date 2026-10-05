# tests/testthat/test-scf_subset.R

test_that("scf_subset matches survey subsetting and handles edge cases", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  sub <- scf_subset(scf2022, age >= 50)
  ref <- subset(scf2022$mi_design[[1]], age >= 50)
  est <- survey::svymean(~networth, sub$mi_design[[1]])
  est_ref <- survey::svymean(~networth, ref)

  expect_equal(coef(est), coef(est_ref))
  expect_equal(survey::SE(est), survey::SE(est_ref))
  expect_null(sub$data)
  expect_null(sub$implicates)

  f <- function(obj, k) scf_subset(obj, age >= k)
  expect_equal(f(scf2022, 50)$n_households, sub$n_households)

  expect_error(scf_subset(scf2022, age > 500), "No rows meet the condition")

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
