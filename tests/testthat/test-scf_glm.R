# tests/testthat/test-scf_glm.R

# NOTE: This test may trigger the benign warning:
#   'non-integer #successes in a binomial glm!'
# This occurs with replicate weights in svyglm(family = binomial()).
# The warning is safe to ignore. See:
# https://stackoverflow.com/questions/12953045/warning-non-integer-successes-in-a-binomial-glm-survey-packages

test_that("scf_glm runs with binomial family (with known warning)", {
  skip_on_cran()

  scf <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))
  scf <- scf_update(scf,
                    log_income = log(pmax(income, 1)))

  model <- suppressWarnings(scf_glm(scf, own ~ age + log_income, family = binomial()))

  expect_s3_class(model, "scf_glm")
  expect_true("results" %in% names(model))
  expect_true(is.data.frame(model$results))
  expect_true(all(c("term", "estimate", "std.error") %in% names(model$results)))
})
