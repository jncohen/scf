# tests/testthat/test-scf_MIcombine.R

test_that("SE works for survey and scf objects", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  m <- survey::svymean(~income, scf2022$mi_design[[1]])
  pooled <- scf_MIcombine(lapply(scf2022$mi_design, function(d) {
    survey::svymean(~income, d)
  }))

  expect_identical(SE, survey::SE)
  expect_equal(SE(m), survey::SE(m))
  expect_equal(unname(SE(pooled)), unname(sqrt(diag(pooled$variance))))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_MIcombine matches mitools, including missinfo", {
  skip_if_not_installed("mitools")

  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  fits <- lapply(scf2022$mi_design, function(d) {
    survey::svyglm(networth ~ age + income, design = d)
  })
  ours <- scf_MIcombine(fits)
  ref <- mitools::MIcombine(fits)

  expect_equal(coef(ours), coef(ref))
  expect_equal(vcov(ours), vcov(ref), ignore_attr = TRUE)
  expect_equal(ours$df, ref$df, ignore_attr = TRUE)
  expect_equal(ours$missinfo, ref$missinfo, ignore_attr = TRUE)
  expect_null(dim(ours$missinfo))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_MIcombine stops with one implicate", {
  expect_error(
    scf_MIcombine(list(c(a = 1)), list(matrix(0.1))),
    "at least two implicates"
  )
})

test_that("scf_MIcombine stops on missing results", {
  res <- list(c(a = 1, b = 2), c(a = 1.1, b = NA), c(a = 0.9, b = 2.1))
  vars <- rep(list(diag(0.1, 2)), 3)
  expect_error(scf_MIcombine(res, vars), "missing")

  res <- list(c(a = 1), c(a = 1.1), c(a = 0.9))
  vars <- list(matrix(0.1), matrix(NA_real_), matrix(0.1))
  expect_error(scf_MIcombine(res, vars), "missing")
})
