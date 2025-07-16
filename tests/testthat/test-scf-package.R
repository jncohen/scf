# tests/testthat/test-scf-package.R

test_that("SCF 2022 mock data loads as scf_mi_survey object", {
  scf <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))
  expect_s3_class(scf, "scf_mi_survey")
  expect_equal(length(scf$mi_design), 5)
})


test_that("Descriptive functions return expected structure", {

  scf <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))
  scf <- scf_update(scf, over50 = age > 50)

  freq <- scf_freq(scf, ~own)
  expect_named(freq, c("results", "imps", "aux"))
  expect_true(inherits(freq, "scf_freq"))

  mean <- scf_mean(scf, ~income)
  expect_named(mean, c("results", "imps", "aux", "verbose"))
  expect_true(inherits(mean, "scf_mean"))

  med <- scf_median(scf, ~income)
  expect_true(inherits(med, "scf_percentile"))
})

test_that("Regression functions return valid model objects", {

  scf <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))
  scf <- scf_update(scf,
                    over50 = factor(age > 50),
                    log_inc = log(pmax(income, 1)),
                    own_bin = factor(own == 1)
  )

  model <- scf_ols(scf, networth ~ income + over50)
  expect_setequal(names(model), c("results", "imps", "fit", "call"))
  expect_true("estimate" %in% names(model$results))

  logit <- scf_logit(scf, own_bin ~ over50 + log_inc)
  expect_setequal(names(logit), c("results", "imps", "fit", "call"))
})


test_that("Plot functions generate ggplot objects", {
  scf <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))

  p1 <- scf_plot_dbar(scf, ~own)
  expect_s3_class(p1, "ggplot")

  p2 <- scf_plot_cbar(scf, ~networth, ~own)
  expect_s3_class(p2, "ggplot")
})
