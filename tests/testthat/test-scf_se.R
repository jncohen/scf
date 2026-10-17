# tests/testthat/test-scf_se.R

load_mock <- function() {
  td <- tempfile()
  dir.create(td)
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  s <- scf_load(2022, data_directory = td)
  unlink(td, recursive = TRUE, force = TRUE)
  s
}

test_that("SE() works on means, percentiles, ratios, and models", {
  s <- load_mock()

  m <- scf_mean(s, ~networth)
  expect_equal(unname(SE(m)), m$results$se)
  expect_named(SE(m), "networth")

  mb <- scf_mean(s, ~networth, by = ~edcl)
  expect_equal(unname(SE(mb)), mb$results$se)
  expect_equal(names(SE(mb)), as.character(mb$results$group))

  md <- scf_median(s, ~income)
  expect_equal(unname(SE(md)), md$results$se)

  r <- scf_ratio(s, ~income, ~networth)
  expect_equal(unname(SE(r)), r$results$se)

  fit <- scf_ols(s, networth ~ income)
  expect_equal(SE(fit), sqrt(diag(as.matrix(vcov(fit)))))
})

test_that("scf_median offers the Fed's stack method", {
  s <- load_mock()
  st <- scf_median(s, ~networth, method = "stack")
  ref <- scf_percentile(s, ~networth, q = 0.5, method = "stack")

  expect_s3_class(st, "scf_median")
  expect_equal(st$results$estimate, ref$results$estimate)
  expect_true(is.na(st$results$se))
  expect_error(scf_median(s, ~networth, method = "other"))

  sb <- scf_median(s, ~networth, by = ~edcl, method = "stack")
  expect_equal(sb$results$estimate,
               scf_percentile(s, ~networth, q = 0.5, by = ~edcl, method = "stack")$results$estimate)
})

test_that("scf_plot_smooth draws without a formula message", {
  s <- load_mock()
  p <- scf_plot_smooth(s, ~age, method = "lm")
  expect_no_message(ggplot2::ggplot_build(p))
})
