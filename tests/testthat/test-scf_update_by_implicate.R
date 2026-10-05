# tests/testthat/test-scf_update_by_implicate.R

test_that("scf_update_by_implicate applies a function to each implicate", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  out <- scf_update_by_implicate(scf2022, function(df) {
    df$rank_nw <- rank(df$networth) / nrow(df)
    df
  })
  for (i in seq_along(out$mi_design)) {
    v <- out$mi_design[[i]]$variables
    expect_equal(v$rank_nw, rank(v$networth) / nrow(v))
  }
  expect_error(scf_update_by_implicate(scf2022, function(df) df[1:5, ]), "Row count mismatch")
  expect_error(scf_update_by_implicate(scf2022, function(df) 1), "must return a data.frame")

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
