# tests/testthat/test-scf_xtab.R

test_that("scf_xtab standard errors include sampling and imputation variance", {
  skip_if_not_installed("mitools")

  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  x <- scf_xtab(scf2022, ~own, ~edcl, variance = "rubin")
  r <- x$results

  ref_cell <- mitools::MIcombine(lapply(scf2022$mi_design, function(d) {
    d$variables$rc <- interaction(factor(d$variables$own), factor(d$variables$edcl))
    survey::svymean(~rc, d)
  }))
  ref_col <- mitools::MIcombine(lapply(scf2022$mi_design, function(d) {
    d$variables$own <- factor(d$variables$own)
    d$variables$edcl <- factor(d$variables$edcl)
    survey::svyby(~own, ~edcl, d, survey::svymean, covmat = TRUE)
  }))
  key <- paste0(r$col, ":own", r$row)

  expect_equal(r$prop, unname(coef(ref_cell)))
  expect_equal(r$se, unname(survey::SE(ref_cell)))
  expect_equal(r$col_share, unname(coef(ref_col)[key]))
  expect_equal(r$col_share_se, unname(survey::SE(ref_col)[key]))
  expect_equal(unname(colSums(x$matrices$col)), rep(1, ncol(x$matrices$col)))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_xtab handles category labels with colons", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  s <- scf_update(scf2022, fac = factor(edcl, labels = c("a:b", "c", "d", "e")))
  x <- scf_xtab(s, ~fac, ~hhsex)
  expect_false(anyNA(x$matrices$row))
  expect_false(anyNA(x$matrices$col))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
