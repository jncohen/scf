# tests/testthat/test-scf_real.R

load_mock <- function() {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf_load(2022, data_directory = td)
}

test_that("scf_real reverses the Fed's adjustment for income and balances", {
  scf2022 <- load_mock()
  scf2022$year <- 2016L
  f_bal <- 4376 / 3548
  f_inc <- f_bal * 3528 / 3483
  scf2022 <- scf_update(scf2022,
    x5729 = income / f_inc,
    x3915 = networth / f_bal
  )

  out <- scf_real(scf2022, ~x5729 + x3915, income = ~x5729)
  v <- out$mi_design[[1]]$variables

  expect_equal(v$x5729_real, v$income)
  expect_equal(v$x3915_real, v$networth)
  expect_equal(v$x5729, scf2022$mi_design[[1]]$variables$x5729)
})

test_that("scf_real leaves the codes -1 and -2 unchanged", {
  scf2022 <- load_mock()
  scf2022$year <- 2016L
  scf2022 <- scf_update(scf2022,
    x3915 = ifelse(seq_along(networth) <= 2, -seq_along(networth), networth)
  )
  v <- scf_real(scf2022, ~x3915)$mi_design[[1]]$variables
  expect_equal(v$x3915_real[1:2], c(-1, -2))
  expect_equal(v$x3915_real[3], v$x3915[3] * 4376 / 3548)
})

test_that("scf_real stops on the wrong kind of variable unless forced", {
  scf2022 <- load_mock()
  scf2022 <- scf_update(scf2022, x8000 = own, x3915 = networth)

  expect_error(scf_real(scf2022, ~income), "not a raw x variable")
  expect_error(scf_real(scf2022, ~x8000), "looks like a code")
  expect_error(scf_real(scf2022, ~x3915, income = ~x5729), "not in `vars`")
  expect_error(scf_real(scf2022, ~x9999), "not in the data")

  done <- scf_real(scf2022, ~x3915)
  expect_error(scf_real(done, ~x3915), "already exists")

  forced <- scf_real(scf2022, ~income, force = TRUE)
  expect_true("income_real" %in% names(forced$mi_design[[1]]$variables))
})

test_that("scf_real converts -1 in income items, as the Fed does", {
  scf2022 <- load_mock()
  scf2022$year <- 2016L
  scf2022 <- scf_update(scf2022,
    x5702 = ifelse(seq_along(income) == 1, -1, income)
  )
  v <- scf_real(scf2022, ~x5702, income = ~x5702)$mi_design[[1]]$variables
  expect_equal(v$x5702_real[1], -1 * 4376 / 3548 * 3528 / 3483)
})
