#' Default Plot Theme for SCF Visualizations
#'
#' Defines the SCF package's default `ggplot2` theme, optimized for legibility,
#' clarity, and aesthetic coherence across print, desktop, and mobile platforms.
#'
#' @description
#' The theme is designed to:
#' - Render cleanly in **print** (single-column or wrapped layout)
#' - Scale well on **HD desktop** monitors without visual clutter
#' - Remain **legible on mobile** with clear fonts and sufficient contrast
#'
#' The default figure dimensions assumed for export are **5.5 inches by 5.5 inches**
#' at **300 dpi**, which balances compactness with accessibility across media.
#'
#' All theme settings are exposed via comments to enable easy brand customization.
#'
#' @param base_size Base font size. Defaults to 13.
#' @param base_family Font family. Defaults to "sans".
#' @param grid Logical. Show gridlines? Defaults to TRUE.
#' @param axis Logical. Include axis ticks and lines? Defaults to TRUE.
#' @param ... Additional arguments passed to [ggplot2::theme_minimal()].
#'
#' @return \code{scf_theme()} returns a \code{ggplot2} theme object.
#'   \code{scf_activate_theme()} returns \code{NULL} invisibly; called for
#'   its side effect of setting the session-wide \code{ggplot2} theme.
#'
#' @section Global activation:
#' \code{scf_activate_theme()} sets \code{scf_theme()} as the default
#' \code{ggplot2} theme for all plots in the current R session — equivalent
#' to \code{ggplot2::theme_set(scf_theme())}. The effect lasts only for the
#' session and does not persist across sessions. All \code{scf_plot_*()} functions
#' apply \code{scf_theme()} internally, so this is most useful when producing
#' custom plots alongside the package's built-in visualizations.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl))) +
#'   geom_bar(fill = "#0072B2") +
#'   scf_theme()
#'
#' # Activate globally for the session:
#' scf_activate_theme()
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

      ## Title styling
      plot.title = ggplot2::element_text(
        face = "bold",
        size = base_size + 2,
        hjust = 0
        # vjust = 1,            # vertical positioning
        # color = "#000000",    # custom title color
      ),

      ## Subtitle styling (if used)
      # plot.subtitle = ggplot2::element_text(
      #   face = "plain",
      #   size = base_size + 0,
      #   hjust = 0
      # ),

      ## Axis titles
      axis.title = ggplot2::element_text(
        size = base_size
        # face = "plain",      # could be "bold", "italic"
        # color = "black"
      ),

      ## Axis text (tick labels)
      axis.text = ggplot2::element_text(
        size = base_size - 1
        # angle = 0,           # optionally tilt text
        # color = "black"
      ),

      ## Axis ticks and lines
      axis.line = if (axis) ggplot2::element_line(color = "grey40") else ggplot2::element_blank(),
      axis.ticks = if (axis) ggplot2::element_line(color = "grey40") else ggplot2::element_blank(),

      ## Grid lines
      panel.grid.major = if (grid) ggplot2::element_line(color = "grey90") else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      # Alternatives:
      # panel.grid.major.x = ggplot2::element_line(color = "grey90"),
      # panel.grid.major.y = ggplot2::element_line(color = "grey95"),

      ## Legend styling (if used)
      # legend.position = "right",   # or "bottom", "top", "none"
      # legend.title = ggplot2::element_text(size = base_size),
      # legend.text = ggplot2::element_text(size = base_size - 1),

      ## Margins (top, right, bottom, left)
      plot.margin = ggplot2::margin(10, 10, 10, 10)

      ## Background options (if dark mode or boxed wanted)
      # plot.background = ggplot2::element_rect(fill = "white", color = NA),
      # panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}
