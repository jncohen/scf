#' Plot Bar Chart of a Discrete Variable from SCF Data
#'
#' Plots the percent of households in each category of a discrete variable.
#'
#' @details
#' Use it to show how households divide across categories, such as education
#' levels. Estimates come from [scf_freq()].
#'
#' @param design A `scf_mi_survey` object created by [scf_load()]. Must contain valid implicates.
#' @param variable A one-sided formula specifying a categorical variable (e.g., `~racecl`).
#' @param title Optional character string for the plot title. Default: `"Distribution of <variable>"`.
#' @param xlab Optional x-axis label. Default: variable name.
#' @param ylab Optional y-axis label. Default: `"Percent"`.
#' @param angle Integer. Rotation angle for x-axis labels. Default is `30`.
#' @param fill Fill color for bars. Default is `"#0072B2"`.
#' @param label_map Optional named vector to relabel x-axis category labels.
#'
#' @return A `ggplot2` object representing the pooled bar chart.
#'
#' @seealso [scf_freq()], [scf_plot_bbar()], [scf_xtab()]
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("plot_dbar_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Bar chart of education categories
#' scf_plot_dbar(scf2022, ~edcl)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @export
scf_plot_dbar <- function(design, variable,
                          title = NULL,
                          xlab = NULL,
                          ylab = "Percent",
                          angle = 30,
                          fill = "#0072B2",
                          label_map = NULL) {

  if (!inherits(variable, "formula")) {
    stop("`variable` must be a one-sided formula, e.g., ~edcl")
  }

  if (!inherits(design, "scf_mi_survey")) {
    stop("Input must be a `scf_mi_survey` object.")
  }

  varname <- all.vars(variable)[1]

  freq <- scf_freq(design, variable, percent = TRUE)
  df <- freq$results

  if (!"proportion" %in% names(df)) {
    stop("`scf_freq()` output must include a `proportion` column.")
  }

  df$category <- factor(df$category, levels = unique(df$category))
  if (!is.null(label_map)) {
    df$category <- .scf_relabel(df$category, label_map)
  }

  df$yval <- df$proportion

  if (is.null(title)) title <- paste("Distribution of", varname)
  if (is.null(xlab)) xlab <- varname

  p <- ggplot2::ggplot(df, ggplot2::aes(x = category, y = yval)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    scf_theme() +
    ggplot2::theme(axis.text.x = .scf_axis_text_x(angle))

  return(p)
}
