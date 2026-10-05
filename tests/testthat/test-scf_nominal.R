# tests/testthat/test-scf_nominal.R

load_mock <- function() {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf_load(2022, data_directory = td)
}

test_that("scf_nominal works on stack percentiles and prints a dollar label", {
  scf2022 <- load_mock()

  st <- scf_percentile(scf2022, ~networth, method = "stack")
  d <- scf_nominal(st, from_year = 2019)
  f <- d$results$estimate / st$results$estimate

  expect_true(isTRUE(attr(d, "nominal")))
  expect_gt(f, 1)

  out <- capture.output(print(d))
  expect_equal(sum(grepl("SCF Percentile Estimate", out)), 1)
  expect_true(any(grepl("nominal 2022 dollars", out)))

  m <- scf_nominal(scf_mean(scf2022, ~networth), from_year = 2019)
  expect_true(any(grepl("nominal 2022 dollars", capture.output(print(m)))))
})

test_that("scf_nominal stops on variables not in 2022 dollars unless forced", {
  scf2022 <- load_mock()
  scf2022 <- scf_update(scf2022, x3915 = networth, nw_k = networth / 1000)

  expect_error(scf_nominal(scf_mean(scf2022, ~x3915)),
               "already in nominal dollars")
  expect_error(scf_nominal(scf_median(scf2022, ~nw_k)),
               "may not be in 2022 dollars")
  expect_error(scf_nominal(scf_ttest(scf2022, ~x3915)),
               "already in nominal dollars")

  forced <- scf_nominal(scf_mean(scf2022, ~nw_k), force = TRUE)
  expect_true(isTRUE(attr(forced, "nominal")))

  real <- scf_real(scf2022, ~x3915)
  expect_silent(scf_nominal(scf_mean(real, ~x3915_real)))
})

test_that("scf_deflate still works but is deprecated", {
  scf2022 <- load_mock()
  m <- scf_mean(scf2022, ~networth)
  expect_warning(d <- scf_deflate(m), "deprecated")
  expect_equal(d$results$estimate, scf_nominal(m)$results$estimate)
})
