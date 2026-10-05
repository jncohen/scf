
test_that("scf_percentile drops only the replicate-weight notice", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  msgs <- character(0)
  withCallingHandlers(
    scf_percentile(scf2022, ~networth, q = 0.5),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_false(any(grepl("Not all replicate weight designs", msgs)))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
