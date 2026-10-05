# tests/testthat/test-scf_corr.R

test_that("scf_corr is weighted and pools replicate and imputation variance", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  x <- scf_corr(scf2022, ~income, ~networth, variance = "rubin")

  per <- lapply(scf2022$mi_design, function(d) {
    xy <- cbind(d$variables$income, d$variables$networth)
    r <- stats::cov.wt(xy, wt = as.numeric(weights(d, "sampling")), cor = TRUE)$cor[1, 2]
    rr <- apply(d$repweights, 2, function(w) {
      stats::cov.wt(xy, wt = as.numeric(w), cor = TRUE)$cor[1, 2]
    })
    c(r = r, v = sum((rr - mean(rr))^2) / (length(rr) - 1))
  })
  r <- sapply(per, function(p) p[["r"]])
  v <- sapply(per, function(p) p[["v"]])
  m <- length(r)
  total <- mean(v) + (1 + 1 / m) * stats::var(r)
  df <- (m - 1) * (1 + mean(v) / ((1 + 1 / m) * stats::var(r)))^2

  expect_equal(x$results$correlation, mean(r))
  expect_equal(x$results$se, sqrt(total))
  expect_equal(x$results$df, df)
  expect_no_error(capture.output(print(x), summary(x)))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_corr uses transformed variables", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  a <- scf_corr(scf2022, ~log(income + 1), ~networth)
  b <- scf_corr(scf2022, ~income, ~networth)
  expect_false(isTRUE(all.equal(a$results$correlation, b$results$correlation)))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
