# tests/testthat/test-scf_logit.R

# NOTE: This test may trigger the benign warning:
#   'non-integer #successes in a binomial glm!'
# This occurs in replicate-weighted logistic regression using `svyglm(family = binomial())`.
# It does not affect estimates or inference.
# See: https://stackoverflow.com/questions/12953045/warning-non-integer-successes-in-a-binomial-glm-survey-packages

test_that("scf_logit runs and returns expected structure (with known warning)", {
  skip_on_cran()

  td  <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)
  
  scf2022 <- scf_update(scf2022,
                    rich = as.integer(networth > 1e6),
                    log_income = log(pmax(income, 1)))

  model <- suppressWarnings(scf_logit(scf2022, rich ~ age + log_income))

  expect_s3_class(model, "scf_logit")
  expect_true("results" %in% names(model))
  expect_true(is.data.frame(model$results))
  expect_true(all(c("term", "estimate", "std.error") %in% names(model$results)))
  
  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
