# tests/testthat/test-scf_implicates.R

test_that("scf_implicates returns implicate-level results for every model type", {
  td <- tempdir()
  src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
  file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
  scf2022 <- scf_load(2022, data_directory = td)

  g <- scf_glm(scf2022, own ~ age)
  q <- scf_quantreg(scf2022, networth ~ age, se = "nid")
  x <- scf_xtab(scf2022, ~own, ~edcl)
  r <- scf_corr(scf2022, ~income, ~networth)

  gi <- scf_implicates(g, long = TRUE)
  qi <- scf_implicates(q, long = TRUE)
  xi <- scf_implicates(x, long = TRUE)
  ri <- scf_implicates(r, long = TRUE)

  expect_equal(nrow(gi), 2 * length(g$models))
  expect_equal(nrow(qi), 2 * length(q$models))
  expect_true(all(is.finite(qi$se)))
  expect_equal(sum(xi$count[xi$implicate == 1]), sum(x$imps[[1]]))
  expect_equal(ri$estimate, unname(r$imps))

  unlink(file.path(td, "scf2022.rds"), force = TRUE)
})
