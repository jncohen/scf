# tests/testthat/test-scf_quantreg.R

test_that("scf_quantreg reports weighted fit statistics and no AIC", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  model <- scf_quantreg(scf2022, networth ~ income + age, se = "nid")

  expect_null(model$fit$AIC)
  expect_error(AIC(model), "AIC not available")

  expect_length(model$fit$nobs, length(model$models))
  expect_equal(model$fit$nobs_mean, nrow(scf2022$mi_design[[1]]$variables))

  w_loss <- sapply(model$models, function(m) {
    r <- m$residuals
    w <- m$weights / mean(m$weights)
    sum(w * r * (0.5 - (r < 0)))
  })
  expect_equal(model$fit$rho, mean(w_loss))
  expect_lte(model$fit$rho, model$fit$rho_null)
  expect_true(model$fit$r1 >= 0 && model$fit$r1 <= 1)

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_quantreg stops when the model fails in an implicate", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)
  scf2022 <- scf_update(scf2022, age2 = age * 2)

  expect_error(
    scf_quantreg(scf2022, networth ~ age + age2, tau = 0.5),
    "implicate 1.*collinear"
  )

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_quantreg replicate variance matches survey::withReplicates", {
  skip_on_cran()
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  m <- scf_quantreg(scf2022, networth ~ age, tau = 0.5, se = "replicate")

  d <- scf2022$mi_design[[1]]
  df <- d$variables
  ref <- survey::withReplicates(d, theta = function(w, ...) {
    df$.wts <- w
    coef(quantreg::rq(networth ~ age, tau = 0.5, data = df,
                                       weights = .wts, method = "fn"))
  })

  expect_true(all(is.finite(m$results$std.error)))
  expect_equal(unname(m$imp_vcov[[1]]), unname(as.matrix(stats::vcov(ref))), ignore_attr = TRUE)

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
