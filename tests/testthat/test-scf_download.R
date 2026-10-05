# tests/testthat/test-scf_download.R

test_that("scf_download ignores years that are not SCF years", {
  expect_identical(scf_download(years = c(1990, 2021), verbose = FALSE), character())
})
