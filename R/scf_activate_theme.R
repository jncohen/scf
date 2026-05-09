#' @rdname scf_theme
#' @export
scf_activate_theme <- function() {
  ggplot2::theme_set(scf_theme())
  message("SCF theme activated for ggplot2 plots.")
}
