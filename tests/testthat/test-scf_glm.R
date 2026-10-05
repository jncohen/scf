# tests/testthat/test-scf_glm.R

# NOTE: This test may trigger the benign warning:
#   'non-integer #successes in a binomial glm!'
# This occurs with replicate weights in svyglm(family = binomial()).
# The warning is safe to ignore. See:
# https://stackoverflow.com/questions/12953045/warning-non-integer-successes-in-a-binomial-glm-survey-packages

test_that("scf_glm runs with binomial family (with known warning)", {
  skip_on_cran()

  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)


  scf2022 <- scf_update(scf2022,
                    log_income = log(pmax(income, 1)))

  model <- scf_glm(scf2022, own ~ age + log_income, family = binomial())

  expect_s3_class(model, "scf_glm")
  expect_true("results" %in% names(model))
  expect_true(is.data.frame(model$results))
  expect_true(all(c("term", "estimate", "std.error") %in% names(model$results)))

  expect_length(model$fit$AIC, 1)
  expect_true(is.finite(model$fit$AIC))
  expect_true(is.finite(model$fit$AIC.sd))
  expect_lt(model$fit$AIC.sd, 0.25 * abs(model$fit$AIC))
  expect_equal(AIC(model), model$fit$AIC)

  expect_length(model$fit$nobs, length(model$models))
  expect_equal(model$fit$nobs_mean, nrow(scf2022$mi_design[[1]]$variables))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)

})

test_that("scf_glm stops when the model fails in an implicate", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)
  scf2022 <- scf_update(scf2022, age2 = age * 2)

  expect_error(
    scf_glm(scf2022, own ~ age + age2, family = binomial()),
    "implicate 1.*collinear"
  )

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_glm passes on model warnings but not the binomial weights warning", {
  skip_on_cran()
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  expect_no_warning(scf_glm(scf2022, own ~ age, family = binomial()))

  for (i in seq_along(scf2022$mi_design)) {
    v <- scf2022$mi_design[[i]]$variables
    scf2022$mi_design[[i]]$variables$sep <- if (i == 1) as.numeric(v$own) * 10 else v$age
  }
  expect_warning(
    scf_glm(scf2022, own ~ sep, family = binomial()),
    "algorithm did not converge (implicate 1)", fixed = TRUE
  )

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_glm stops when a factor level is missing from an implicate", {
  skip_on_cran()
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
    scf_glm(scf2022, own ~ grp, family = binomial()),
    "`grpc` is missing from implicate 2"
  )

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
