#' Activate SCF Plot Theme
#'
#' Sets the default `ggplot2` theme to `scf_theme()`. 
#' Call this function manually in your session or script to apply the style globally.
#'
#' @export
scf_activate_theme <- function() {
  ggplot2::theme_set(scf_theme())
  message("SCF theme activated for ggplot2 plots.")
}
