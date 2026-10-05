#' Default Plot Theme for SCF Visualizations
#'
#' The `ggplot2` theme used by all `scf_plot_*()` functions.
#'
#' @details
#' Based on `theme_minimal()` and sized for 5.5 by 5.5 inch figures at 300
#' dpi. Add a `ggplot2::theme()` call to change settings.
#' [scf_activate_theme()] sets it as the session default.
#'
#' @param base_size Base font size. Defaults to 13.
#' @param base_family Font family. Defaults to "sans".
#' @param grid Logical. Show gridlines? Defaults to TRUE.
#' @param axis Logical. Include axis ticks and lines? Defaults to TRUE.
#' @param ... Additional arguments passed to [ggplot2::theme_minimal()].
#'
#' @return `scf_theme()` returns a `ggplot2` theme object.
#'   `scf_activate_theme()` returns `NULL` invisibly; called for
#'   its side effect of setting the session-wide `ggplot2` theme.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl))) +
#'   geom_bar(fill = "#0072B2") +
#'   scf_theme()
#'
#' # Activate globally for the session, then restore the previous theme:
#' old <- ggplot2::theme_get()
#' scf_activate_theme()
#' ggplot2::theme_set(old)
#'
#' @seealso [ggplot2::theme()], [scf_plot_dist()]
#' @name scf_theme
#' @export
scf_theme <- function(base_size = 13,
                      base_family = "sans",
                      grid = TRUE,
                      axis = TRUE,
                      ...) {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family, ...) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2, hjust = 0),
      axis.title = ggplot2::element_text(size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 1),
      axis.line = if (axis) ggplot2::element_line(color = "grey40") else ggplot2::element_blank(),
      axis.ticks = if (axis) ggplot2::element_line(color = "grey40") else ggplot2::element_blank(),
      panel.grid.major = if (grid) ggplot2::element_line(color = "grey90") else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}
