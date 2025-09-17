# tests/testthat/test-scf_glm.R

# NOTE: This test may trigger the benign warning:
#   'non-integer #successes in a binomial glm!'
# This occurs with replicate weights in svyglm(family = binomial()).
# The warning is safe to ignore. See:
# https://stackoverflow.com/questions/12953045/warning-non-integer-successes-in-a-binomial-glm-survey-packages

test_that("scf_glm runs with binomial family (with known warning)", {
  skip_on_cran()

  td  <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)
  
  
  scf2022 <- scf_update(scf2022,
                    log_income = log(pmax(income, 1)))

  model <- suppressWarnings(scf_glm(scf2022, own ~ age + log_income, family = binomial()))

  expect_s3_class(model, "scf_glm")
  expect_true("results" %in% names(model))
  expect_true(is.data.frame(model$results))
  expect_true(all(c("term", "estimate", "std.error") %in% names(model$results)))
  
  unlink("scf2022.rds", force = TRUE)
})
