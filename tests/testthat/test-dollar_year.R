# tests/testthat/test-dollar_year.R

load_mock <- function() {
  td <- tempfile()
  dir.create(td)
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  s <- scf_load(2022, data_directory = td)
  unlink(td, recursive = TRUE, force = TRUE)
  s
}

test_that("CPI tables cover every SCF year and match the Fed's macro for 2025", {
  expect_equal(names(scf:::.scf_cpi_urs), as.character(scf:::.scf_years))
  expect_equal(names(scf:::.scf_cpi_lag), as.character(scf:::.scf_years))
  expect_equal(unname(scf:::.scf_cpi_urs["2025"]), 4789)
  expect_equal(unname(scf:::.scf_cpi_lag["2025"]), 4750 / 4625)
  expect_equal(unname(scf:::.scf_cpi_urs["2022"]), 4376)
})

test_that("the dollar year is found from wageinc and x5702", {
  cpi <- scf:::.scf_cpi_urs
  lag <- scf:::.scf_cpi_lag
  df <- data.frame(x5702 = c(0, 10000, 52000, 87000))
  df$wageinc <- df$x5702 * lag[["2016"]] * cpi[["2025"]] / cpi[["2016"]]
  expect_equal(scf:::.scf_detect_dollar_year(df, 2016), 2025L)

  df$wageinc <- df$x5702 * lag[["2016"]] * cpi[["2022"]] / cpi[["2016"]]
  expect_equal(scf:::.scf_detect_dollar_year(df, 2016), 2022L)

  df$wageinc <- df$x5702 * 1.5
  expect_true(is.na(scf:::.scf_detect_dollar_year(df, 2016)))
  expect_true(is.na(scf:::.scf_detect_dollar_year(data.frame(income = 1), 2016)))
})

test_that("scf_load, scf_design, and scf_subset carry the dollar year", {
  s <- load_mock()
  expect_equal(s$dollar_year, 2022L)
  expect_true(any(grepl("Dollar year:    2022", capture.output(print(s)), fixed = TRUE)))

  obj <- scf_design(s$mi_design, 2022, s$n_households, dollar_year = 2025)
  expect_equal(obj$dollar_year, 2025L)
  expect_true(is.na(scf_design(s$mi_design, 2022, s$n_households)$dollar_year))

  expect_equal(scf_subset(s, age >= 50)$dollar_year, 2022L)
  expect_equal(scf_update(s, a = age)$dollar_year, 2022L)
  expect_equal(scf_mean(s, ~networth)$aux$dollar_year, 2022L)
  expect_equal(scf_median(s, ~networth)$aux$dollar_year, 2022L)
  expect_equal(scf_ttest(s, ~networth, mu = 0)$aux$dollar_year, 2022L)
})

test_that("scf_load warns when years are in different dollar years", {
  td <- tempfile()
  dir.create(td)
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"))
  m <- readRDS(src)
  attr(m, "dollar_year") <- 2025L
  saveRDS(m, file.path(td, "scf2019.rds"))

  expect_warning(s <- scf_load(2019, 2022, data_directory = td),
                 "different dollar years")
  expect_equal(s[["2019"]]$dollar_year, 2025L)

  unlink(td, recursive = TRUE, force = TRUE)
})

test_that("scf_real converts to the data's dollar year", {
  s <- load_mock()
  s$year <- 2016L
  s$dollar_year <- 2025L
  f_bal <- 4789 / 3548
  f_inc <- f_bal * 3528 / 3483
  s <- scf_update(s, x5729 = income / f_inc, x3915 = networth / f_bal)

  v <- scf_real(s, ~x5729 + x3915, income = ~x5729)$mi_design[[1]]$variables
  expect_equal(v$x5729_real, v$income)
  expect_equal(v$x3915_real, v$networth)

  expect_error(scf_real(s, ~income), "already in 2025 dollars")

  s$dollar_year <- NA_integer_
  expect_error(scf_real(s, ~x3915), "dollar year of these data is unknown")
})

test_that("scf_nominal converts from the data's dollar year by default", {
  s <- load_mock()
  m <- scf_mean(s, ~networth)
  expect_equal(scf_nominal(m)$results$estimate, m$results$estimate)

  s$dollar_year <- 2025L
  m <- scf_mean(s, ~networth)
  n <- scf_nominal(m)
  expect_equal(n$results$estimate, m$results$estimate * 4376 / 4789)
  expect_equal(attr(n, "from_year"), 2025L)
  expect_equal(scf_nominal(m, from_year = 2022)$results$estimate, m$results$estimate)

  t <- scf_ttest(s, ~networth, mu = 0)
  expect_equal(scf_nominal(t)$results$estimate, t$results$estimate * 4376 / 4789)

  s$dollar_year <- NA_integer_
  expect_error(scf_nominal(scf_mean(s, ~networth)), "dollar year of this result is unknown")
})
