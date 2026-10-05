# tests/testthat/test-scf_theme.R

test_that("scf_theme returns a ggplot2 theme and scf_activate_theme sets it", {
  th <- scf_theme()
  expect_s3_class(th, "theme")
  expect_s3_class(scf_theme(grid = FALSE, axis = FALSE)$panel.grid.major, "element_blank")

  old <- ggplot2::theme_get()
  expect_message(scf_activate_theme(), "SCF theme activated")
  expect_equal(ggplot2::theme_get()$plot.title$face, "bold")
  ggplot2::theme_set(old)
})
