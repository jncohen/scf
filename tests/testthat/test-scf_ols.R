# tests/testthat/test-scf_ols.R

test_that("scf_ols reports a single per-implicate-mean AIC", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  model <- scf_ols(scf2022, networth ~ income + age)
  per_imp <- sapply(model$imps, function(m) AIC(m)[["AIC"]])

  expect_length(model$fit$AIC, 1)
  expect_true(is.finite(model$fit$AIC))
  expect_equal(model$fit$AIC, mean(per_imp))
  expect_equal(model$fit$AIC.sd, sd(per_imp))
  # Implicates differ only by imputation; spread should be small relative to level
  expect_lt(model$fit$AIC.sd, 0.1 * abs(model$fit$AIC))
  expect_equal(AIC(model), model$fit$AIC)

  expect_length(model$fit$nobs, length(model$imps))
  expect_equal(model$fit$nobs_mean, nrow(scf2022$mi_design[[1]]$variables))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_ols stops when the model fails in an implicate", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)
  scf2022 <- scf_update(scf2022, age2 = age * 2)

  expect_error(
    scf_ols(scf2022, networth ~ age + age2),
    "implicate 1.*collinear"
  )

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_ols stops when a factor level is missing from an implicate", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)
  for (i in seq_along(scf2022$mi_design)) {
    v <- scf2022$mi_design[[i]]$variables
    g <- ifelse(v$age > 50, "a", "b")
    if (i == 1) g[1:60] <- "c"
    scf2022$mi_design[[i]]$variables$grp <- factor(g)
  }

  expect_error(
    scf_ols(scf2022, networth ~ grp),
    "`grpc` is missing from implicate 2"
  )

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
