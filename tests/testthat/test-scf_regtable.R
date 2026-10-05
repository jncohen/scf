test_that("scf_regtable handles mixed model types", {
  skip_on_cran()

  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)
  scf2022 <- scf_update(scf2022, log_income = log(pmax(income, 1)))

  o <- scf_ols(scf2022, networth ~ age + log_income)
  l <- scf_logit(scf2022, own ~ age + log_income)
  g <- scf_glm(scf2022, own ~ age + log_income)
  q <- scf_quantreg(scf2022, networth ~ age + log_income, se = "nid")

  invisible(capture.output(tab <- scf_regtable(o, l, g, q)))
  fit_rows <- tab[match(c("N", "R2", "PseudoR2", "Tau", "R1", "R1(adj)", "AIC"), tab$Term), ]

  expect_false(anyNA(fit_rows$Term))
  expect_true(all(fit_rows[fit_rows$Term == "N", -1] == "200"))
  expect_equal(unlist(fit_rows[fit_rows$Term == "AIC", -1], use.names = FALSE) != "--",
               c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(unlist(fit_rows[fit_rows$Term == "R2", -1], use.names = FALSE),
               c("0.139", "--", "--", "--"))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_regtable escapes LaTeX special characters", {
  m <- structure(
    list(
      results = data.frame(term = c("(Intercept)", "log_income"),
                           estimate = c(1, 2), std.error = c(0.5, 1),
                           stars = c("***", "^"), stringsAsFactors = FALSE),
      fit = list(nobs_mean = 100, r.squared = 0.5, AIC = 10)
    ),
    class = c("scf_ols", "scf_model_result")
  )

  tex <- capture.output(scf_regtable(m, model.names = "Model_1", output = "latex", digits = 0))

  expect_true(any(grepl("log\\_income", tex, fixed = TRUE)))
  expect_true(any(grepl("Model\\_1", tex, fixed = TRUE)))
  expect_true(any(grepl("2\\^{} (1)", tex, fixed = TRUE)))
  expect_false(any(grepl("log_income", tex, fixed = TRUE)))
  expect_false(any(grepl("2^ (1)", tex, fixed = TRUE)))
})

test_that("scf_regtable markdown escapes stars and keeps term order", {
  m <- structure(
    list(
      results = data.frame(term = c("(Intercept)", "zeta", "alpha"),
                           estimate = c(1, 0.02, 2500), std.error = c(0.5, 0.01, 100),
                           stars = c("***", "^", ""), stringsAsFactors = FALSE),
      fit = list(nobs_mean = 100, r.squared = 0.5, AIC = 10)
    ),
    class = c("scf_ols", "scf_model_result")
  )

  md <- capture.output(out <- scf_regtable(m, output = "markdown"))
  expect_true(any(grepl("0.020\\^ (0.010)", md, fixed = TRUE)))
  expect_false(any(grepl("^{}", md, fixed = TRUE)))
  expect_equal(out$Term[1:3], c("(Intercept)", "zeta", "alpha"))
  expect_true(any(grepl("2500 (100.00)", md, fixed = TRUE)))
})
