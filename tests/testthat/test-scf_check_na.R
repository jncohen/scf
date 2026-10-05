# tests/testthat/test-scf_check_na.R

test_that("functions stop with a clear error when a variable has missing values", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  s <- scf_update(scf2022,
    log_nw = log(ifelse(networth > 0, networth, NA)))

  msg <- "has [0-9]+ missing values"
  expect_error(scf_mean(s, ~log_nw), msg)
  expect_error(scf_freq(s, ~own, by = ~log_nw), msg)
  expect_error(scf_ols(s, log_nw ~ age), msg)
  expect_error(expect_warning(scf_ols(s, income ~ log(networth)), "NaNs produced"), msg)
  expect_error(scf_quantreg(s, log_nw ~ age), msg)
  expect_error(scf_plot_hist(s, ~log_nw), "`log_nw`")

  pos <- scf_subset(s, networth > 0)
  expect_no_error(scf_mean(pos, ~log_nw))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
