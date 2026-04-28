#' Activate SCF Plot Theme Globally
#'
#' @description
#' Sets `scf_theme()` as the default `ggplot2` theme for all plots in the
#' current R session. Call this once at the top of an analysis script to avoid
#' adding `+ scf_theme()` to every individual plot.
#'
#' @details
#' There are two ways to apply the SCF theme:
#'
#' \describe{
#'   \item{Per-plot}{Add `+ scf_theme()` to each individual plot. Useful when
#'     mixing SCF-styled plots with other themes in the same script.}
#'   \item{Global (this function)}{Call `scf_activate_theme()` once and all
#'     subsequent `ggplot2` plots in the session will use the SCF theme
#'     automatically. Equivalent to calling
#'     `ggplot2::theme_set(scf_theme())` directly.}
#' }
#'
#' The effect lasts only for the current R session and does not persist across
#' sessions. All `scf_plot_*()` functions apply `scf_theme()` internally, so
#' this function is most useful when producing custom plots alongside the
#' package's built-in visualizations.
#'
#' @return No return value; called for its side effect of setting the global
#'   ggplot2 theme.
#'
#' @seealso [scf_theme()]
#'
#' @examples
#' scf_activate_theme()
#'
#' @export
scf_activate_theme <- function() {
  ggplot2::theme_set(scf_theme())
  message("SCF theme activated for ggplot2 plots.")
}
