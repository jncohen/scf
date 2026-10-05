# tests/testthat/test-scf_logit.R

# NOTE: This test may trigger the benign warning:
#   'non-integer #successes in a binomial glm!'
# This occurs in replicate-weighted logistic regression using `svyglm(family = binomial())`.
# It does not affect estimates or inference.
# See: https://stackoverflow.com/questions/12953045/warning-non-integer-successes-in-a-binomial-glm-survey-packages

test_that("scf_logit runs and returns expected structure (with known warning)", {
  skip_on_cran()

  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  scf2022 <- scf_update(scf2022,
                    rich = as.integer(networth > 1e6),
                    log_income = log(pmax(income, 1)))

  expect_warning(
    model <- scf_logit(scf2022, rich ~ age + log_income),
    "fitted probabilities numerically 0 or 1"
  )

  expect_s3_class(model, "scf_logit")
  expect_true("results" %in% names(model))
  expect_true(is.data.frame(model$results))
  expect_true(all(c("term", "estimate", "std.error") %in% names(model$results)))

  expect_length(model$fit$AIC, 1)
  expect_true(is.finite(model$fit$AIC))
  expect_true(is.finite(model$fit$AIC.sd))
  expect_lt(model$fit$AIC.sd, 0.25 * abs(model$fit$AIC))
  expect_equal(AIC(model), model$fit$AIC)

  expect_length(model$fit$nobs, length(model$imps))
  expect_equal(model$fit$nobs_mean, nrow(scf2022$mi_design[[1]]$variables))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("summary works for scf_logit with odds = FALSE", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  m <- scf_logit(scf2022, I(hhsex == 1) ~ age, odds = FALSE)
  expect_no_error(capture.output(summary(m), print(m)))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("confint for odds ratios exponentiates the log-odds interval", {
  skip_on_cran()
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  lo <- scf_logit(scf2022, own ~ age, odds = FALSE)
  or <- scf_logit(scf2022, own ~ age, odds = TRUE)

  expect_equal(confint(or), exp(confint(lo)))
  q <- qt(0.975, lo$df)
  expect_equal(unname(confint(lo)[, 1]), lo$results$estimate - q * lo$results$std.error)

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
