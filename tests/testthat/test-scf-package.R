# tests/testthat/test-scf-package.R

test_that("scf2022 mock data loads as scf_mi_survey object", {
  td  <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)
  
  expect_s3_class(scf2022, "scf_mi_survey")
  expect_equal(length(scf2022$mi_design), 5)
  
  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("Descriptive functions return expected structure", {
  td  <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)
  
  scf2022 <- scf_update(scf2022, over50 = age > 50)
  
  freq <- scf_freq(scf2022, ~own)
  expect_named(freq, c("results", "imps", "aux"))
  expect_true(inherits(freq, "scf_freq"))
  
  mean <- scf_mean(scf2022, ~income)
  expect_named(mean, c("results", "imps", "aux", "verbose"))
  expect_true(inherits(mean, "scf_mean"))
  
  med <- scf_median(scf2022, ~income)
  expect_true(inherits(med, "scf_percentile"))
  
  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("Regression functions return valid model objects", {
  td  <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)
  
  scf2022 <- scf_update(scf2022,
                        over50 = factor(age > 50),
                        log_inc = log(pmax(income, 1)),
                        own_bin = factor(own == 1)
  )
  
  model <- scf_ols(scf2022, networth ~ income + over50)
  expect_setequal(names(model), c("results", "imps", "fit", "call"))
  expect_true("estimate" %in% names(model$results))
  
  logit <- scf_logit(scf2022, own_bin ~ over50 + log_inc)
  expect_setequal(names(logit), c("results", "imps", "fit", "call"))
  
  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("Plot functions generate ggplot objects", {
  td  <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)
  
  p1 <- scf_plot_dbar(scf2022, ~own)
  expect_s3_class(p1, "ggplot")
  
  p2 <- scf_plot_cbar(scf2022, ~networth, ~own)
  expect_s3_class(p2, "ggplot")
  
  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
