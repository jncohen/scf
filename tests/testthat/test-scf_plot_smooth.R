# tests/testthat/test-scf_plot_smooth.R

test_that("scf_plot_smooth uses sampling weights and accepts expressions", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  p <- suppressMessages(scf_plot_smooth(scf2022, ~age, binwidth = 5))
  n <- sum(sapply(scf2022$mi_design, function(d) nrow(d$variables)))
  expect_lt(nrow(p$data), n)
  expect_equal(sum(p$data$percent), 100)

  q <- suppressMessages(scf_plot_smooth(scf2022, ~log(income + 1)))
  expect_equal(q$labels$x, "log(income + 1)")

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
