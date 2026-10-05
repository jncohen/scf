# tests/testthat/test-scf_update.R

test_that("scf_update creates variables and reports bad input", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  k <- 50
  out <- scf_update(scf2022, over_k = age > k, one = 1, log_inc = log(income + 1))
  v <- out$mi_design[[1]]$variables
  expect_equal(v$over_k, v$age > 50)
  expect_true(all(v$one == 1))
  expect_true("log_inc" %in% names(v))

  dropped <- scf_update(out, one = NULL)
  expect_false("one" %in% names(dropped$mi_design[[1]]$variables))

  expect_error(scf_update(scf2022, z = not_a_variable + 1), "Could not create `z`")
  expect_error(scf_update(scf2022, w = 1:3), "must have length 1 or")
  expect_error(scf_update(scf2022, age > 50), "must be named")

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})

test_that("scf_update accepts names that start like its first argument", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  out <- scf_update(scf2022, o = as.integer(own == 1), obj = 1)
  expect_true(all(c("o", "obj") %in% names(out$mi_design[[1]]$variables)))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
