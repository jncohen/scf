# tests/testthat/test-scf_mean.R

test_that("grouped results keep groups missing from some implicates", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  for (imp in c(1, 2)) {
    s <- scf2022
    v <- s$mi_design[[imp]]$variables
    v$edcl[v$edcl == 1] <- 2
    s$mi_design[[imp]] <- scf:::.scf_svrepdesign(v)

    m <- scf_mean(s, ~networth, by = ~edcl)
    med <- scf_median(s, ~networth, by = ~edcl)
    f <- scf_freq(s, ~edcl)

    expect_equal(m$results$group, c("1", "2", "3", "4"))
    expect_true(all(is.finite(m$results$se)))
    expect_equal(med$results$group, c("1", "2", "3", "4"))
    expect_equal(sum(f$results$proportion), 100)
  }

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_mean pools each of several variables", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  m <- scf_mean(scf2022, ~income + networth)
  one <- scf_mean(scf2022, ~networth)
  expect_equal(m$results$variable, c("income", "networth"))
  expect_equal(m$results$estimate[2], one$results$estimate)
  expect_equal(m$results$se[2], one$results$se)
  expect_error(scf_mean(scf2022, ~income + networth, by = ~edcl),
               "one variable")

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
