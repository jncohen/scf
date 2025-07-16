#' Stacked Bar Chart of Two Discrete Variables in SCF Data
#'
#' @description
#' Visualizes a discrete-discrete bivariate distribution using stacked bars
#' based on pooled cross-tabulations from [scf_xtab()]. Use this function to
#' visualize the relationship between two discrete variables.
#'
#' @section Implementation:
#' This function calls [scf_xtab()] to estimate the joint distribution of two
#' categorical variables across multiply-imputed SCF data. The result is translated
#' into a `ggplot2` stacked bar chart using pooled counts or normalized percentages.
#'
#' @param design A `scf_mi_survey` object created by [scf_load()]. Must contain five implicates with replicate weights.
#' @param rowvar A one-sided formula for the x-axis grouping variable (e.g., `~edcl`).
#' @param colvar A one-sided formula for the stacked fill variable (e.g., `~racecl`).
#' @param scale Character. One of `"percent"` (default) or `"count"`.
#' @param percent_by Character. One of `"total"` (default), `"row"`, or `"col"` — determines normalization base when `scale = "percent"`.
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
#' \donttest{
#' # Load bundled mock data (for demonstration only — not real SCF data)
#' scf2022 <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))

#' # For real analysis, use:
#' # scf_download(2022); scf2022 <- scf_load(2022)
#'
#'
#' # Stacked bar chart: education by race
#' scf_plot_bbar(scf2022, ~edcl, ~racecl)
#'
#' # Column percentages instead of total percent
#' scf_plot_bbar(scf2022, ~edcl, ~racecl, percent_by = "col")
#'
#' # Raw counts (estimated number of households)
#' scf_plot_bbar(scf2022, ~edcl, ~racecl, scale = "count")
#' }
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

  if (isTRUE(attr(design, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }

  `%||%` <- function(a, b) if (!is.null(a)) a else b
  rowname <- deparse(rowvar[[2]])
  colname <- deparse(colvar[[2]])

  xtab <- scf_xtab(design, rowvar, colvar)
  df <- xtab$results

  # Choose y variable and label
  if (scale == "count") {
    df$yval <- df$prop * sum(sapply(xtab$imps, sum))
    ylab <- ylab %||% "Estimated Count"
  } else {
    ylab <- ylab %||% "Percent"
    df$yval <- switch(percent_by,
                      total = df$prop,
                      row = df$row_share,
                      col = df$col_share) * 100
  }

  # Convert to factor preserving order of appearance
  df$row <- factor(df$row, levels = unique(df$row))
  df$col <- factor(df$col, levels = unique(df$col))

  # Relabel if requested
  if (!is.null(row_labels)) {
    levels(df$row) <- unname(row_labels[levels(df$row)])
  }
  if (!is.null(col_labels)) {
    levels(df$col) <- unname(col_labels[levels(df$col)])
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = row, y = yval, fill = col)) +
    ggplot2::geom_col(position = "stack", color = "white") +
    ggplot2::labs(
      title = title,
      x = xlab %||% rowname,
      y = ylab,
      fill = colname
    ) +
    scf_theme()

  if (!is.null(fill_colors)) {
    p <- p + ggplot2::scale_fill_manual(values = fill_colors)
  }

  return(p)
}
