# tests/testthat/test-scf_model_methods.R

test_that("predict works with and without newdata", {
  skip_on_cran()

  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)
  scf2022 <- scf_update(scf2022, log_income = log(pmax(income, 1)))
  nd <- scf2022$mi_design[[1]]$variables[1:4, ]
  n <- nrow(scf2022$mi_design[[1]]$variables)

  o <- scf_ols(scf2022, networth ~ age + log_income + factor(edcl))
  l <- scf_logit(scf2022, own ~ age + log_income)
  q <- scf_quantreg(scf2022, networth ~ age + log_income, se = "nid")

  expect_null(o$imps[[1]]$data)
  expect_null(l$imps[[1]]$data)

  expect_length(predict(o), n)
  expect_length(predict(o, newdata = nd), 4)
  expect_equal(unname(predict(o, newdata = nd)),
               unname(rowMeans(sapply(o$imps, stats::predict.glm, newdata = nd))))

  p <- predict(l, newdata = nd, type = "response")
  expect_length(p, 4)
  expect_true(all(p >= 0 & p <= 1))

  expect_length(predict(q), n)
  expect_length(predict(q, newdata = nd), 4)

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
