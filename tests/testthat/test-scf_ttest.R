# tests/testthat/test-scf_ttest.R

test_that("summary works for a two-sample scf_ttest", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  scf2022 <- scf_update(scf2022,
                        female = factor(hhsex == 2, labels = c("Male", "Female")))

  model <- scf_ttest(scf2022, ~income, group = ~female)

  expect_no_error(capture.output(summary(model)))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("two-sample scf_ttest accounts for covariance between groups", {
  skip_if_not_installed("mitools")

  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  scf2022 <- scf_update(scf2022,
                        female = factor(hhsex == 2, labels = c("Male", "Female")))

  model <- scf_ttest(scf2022, ~income, group = ~female,
                                      variance = "rubin")
  ref <- mitools::MIcombine(lapply(scf2022$mi_design, function(d) {
    by <- survey::svyby(~income, ~female, d, survey::svymean, covmat = TRUE)
    survey::svycontrast(by, c(1, -1))
  }))

  expect_equal(model$results$estimate, unname(coef(ref)))
  expect_equal(model$results$std.error, unname(survey::SE(ref)))
  expect_equal(model$results$df, unname(ref$df))
  expect_true(any(grepl("difference in means", capture.output(print(model)))))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_ttest uses transformed variables", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  t <- scf_ttest(scf2022, ~log(income + 1))
  ref <- mean(sapply(scf2022$mi_design, function(d) {
    coef(survey::svymean(~log(income + 1), d))
  }))
  expect_equal(t$results$estimate, ref)

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_ttest stops when a group is missing from an implicate", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)
  for (i in seq_along(scf2022$mi_design)) {
    v <- scf2022$mi_design[[i]]$variables
    scf2022$mi_design[[i]]$variables$grp <- if (i == 2) rep("a", nrow(v)) else ifelse(v$age > 50, "a", "b")
  }
  expect_error(scf_ttest(scf2022, ~income, group = ~grp),
               "implicate 2 has only one")

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
