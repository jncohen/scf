# tests/testthat/test-scf_pctile_sum.R

test_that("scf_pctile_sum handles tied values at group boundaries", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  out <- scf_pctile_sum(scf2022, ~own, probs = c(0, 0.1, 0.2, 1),
                                         stat = "none")
  for (d in out$mi_design) {
    w <- as.numeric(weights(d, "sampling"))
    share <- tapply(w, d$variables$own_pctile, sum) / sum(w)
    expect_equal(names(share), c("p0-p10", "p10-p20", "p20-p100"))
    expect_true(all(abs(share - c(0.1, 0.1, 0.8)) < 0.05))
  }

  res <- scf_pctile_sum(scf2022, ~own, probs = c(0, 0.05, 0.1, 1))
  expect_equal(nrow(res$results), 3)

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_pctile_cut is deprecated and calls scf_pctile_sum", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  new <- scf_pctile_sum(scf2022, ~networth, probs = c(0, 0.5, 1), method = "stack")
  expect_warning(
    old <- scf_pctile_cut(scf2022, ~networth, probs = c(0, 0.5, 1), method = "stack"),
    "deprecated"
  )
  expect_equal(old, new)

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
