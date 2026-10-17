#' Stacked Bar Chart of Two Discrete Variables in SCF Data
#'
#' Plots a pooled two-way table as stacked bars.
#'
#' @details
#' Use it to show how two discrete variables relate, such as homeownership by
#' education. Each bar is a category of `rowvar`, split by `colvar`. Heights
#' are percentages of all households, or of each row or column
#' (`percent_by`), or estimated counts (`scale = "count"`). Estimates come
#' from [scf_xtab()].
#'
#' @param design A `scf_mi_survey` object from [scf_load()].
#' @param rowvar A one-sided formula for the x-axis grouping variable (e.g., `~edcl`).
#' @param colvar A one-sided formula for the stacked fill variable (e.g., `~racecl`).
#' @param scale Character. One of `"percent"` (default) or `"count"`.
#' @param percent_by Character. One of `"total"` (default), `"row"`, or `"col"`. Sets what the percentages are shares of when `scale = "percent"`.
#' @param title Optional character string for the plot title.
#' @param xlab Optional character string for the x-axis label.
#' @param ylab Optional character string for the y-axis label.
#' @param fill_colors Optional vector of fill colors to pass to `ggplot2::scale_fill_manual()`.
#' @param row_labels Optional named vector to relabel `row` categories (x-axis).
#' @param col_labels Optional named vector to relabel `col` categories (legend).
#'
#' @return A `ggplot2` object.
#'
#' @examples
#' \dontshow{
#' td <- tempfile()
#' dir.create(td)
#' file.copy(system.file("extdata", "scf2022_mock_raw.rds", package = "scf"),
#'           file.path(td, "scf2022.rds"))
#' scf2022 <- scf_load(2022, data_directory = td)
#' }
#' # Stacked bar chart: education by ownership
#' scf_plot_bbar(scf2022, ~own, ~edcl)
#'
#' \donttest{
#' # Column percentages instead of total percent
#' scf_plot_bbar(scf2022, ~own, ~edcl, percent_by = "col")
#'
#' # Raw counts (estimated number of households)
#' scf_plot_bbar(scf2022, ~own, ~edcl, scale = "count")
#' }
#' \dontshow{unlink(td, recursive = TRUE, force = TRUE)}
#'
#' @export
scf_plot_bbar <- function(design, rowvar, colvar,
                          scale = c("percent", "count"),
                          percent_by = c("total", "row", "col"),
                          title = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          fill_colors = NULL,
                          row_labels = NULL,
                          col_labels = NULL) {

  scale <- match.arg(scale)
  percent_by <- match.arg(percent_by)

  rowname <- deparse(rowvar[[2]])
  colname <- deparse(colvar[[2]])

  xtab <- scf_xtab(design, rowvar, colvar)
  df <- xtab$results

  if (scale == "count") {
    df$yval <- df$prop * sum(sapply(xtab$imps, sum)) / length(xtab$imps)
    if (is.null(ylab)) ylab <- "Estimated Count"
  } else {
    if (is.null(ylab)) ylab <- "Percent"
    df$yval <- switch(percent_by,
                      total = df$prop,
                      row = df$row_share,
                      col = df$col_share) * 100
  }

  df$row <- factor(df$row, levels = unique(df$row))
  df$col <- factor(df$col, levels = unique(df$col))

  if (!is.null(row_labels)) {
    df$row <- .scf_relabel(df$row, row_labels)
  }
  if (!is.null(col_labels)) {
    df$col <- .scf_relabel(df$col, col_labels)
  }

  if (is.null(xlab)) xlab <- rowname

  p <- ggplot2::ggplot(df, ggplot2::aes(x = row, y = yval, fill = col)) +
    ggplot2::geom_col(position = "stack", color = "white") +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab,
      fill = colname
    ) +
    scf_theme()

  if (!is.null(fill_colors)) {
    p <- p + ggplot2::scale_fill_manual(values = fill_colors)
  }

  return(p)
}
