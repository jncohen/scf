# tests/testthat/test-scf_logit.R

# NOTE: This test may trigger the benign warning:
#   'non-integer #successes in a binomial glm!'
# This occurs in replicate-weighted logistic regression using `svyglm(family = binomial())`.
# It does not affect estimates or inference.
# See: https://stackoverflow.com/questions/12953045/warning-non-integer-successes-in-a-binomial-glm-survey-packages

test_that("scf_logit runs and returns expected structure (with known warning)", {
  skip_on_cran()

  scf <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))
  scf <- scf_update(scf,
                    rich = as.integer(networth > 1e6),
                    log_income = log(pmax(income, 1)))

  model <- suppressWarnings(scf_logit(scf, rich ~ age + log_income))

  expect_s3_class(model, "scf_logit")
  expect_true("results" %in% names(model))
  expect_true(is.data.frame(model$results))
  expect_true(all(c("term", "estimate", "std.error") %in% names(model$results)))
})
