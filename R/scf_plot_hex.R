#' Hexbin Plot of Two Continuous SCF Variables

#' @description
#' Visualizes the bivariate relationship between two continuous SCF variables
#' using hexagonal bins. 
#'
#' @section Implementation:
#' The function stacks all implicates into one data frame, retains replicate weights,
#' and uses `ggplot2::geom_hex()` to produce a density-style scatterplot. The color
#' intensity of each hexagon reflects the Rubin-pooled weighted count of households
#' in that cell. Missing values are excluded.
#'
#' This plot is especially useful for visualizing joint distributions with large
#' samples and skewed marginals, such as net worth vs. income.
#'
#' @section Aesthetic Guidance:
#' This plot uses a log-scale fill and `viridis` palette to highlight variation
#' in density. To adjust the visual style globally, use [scf_theme()] or set it
#' explicitly with `ggplot2::theme_set(scf_theme())`. For mobile-friendly or
#' publication-ready appearance, export the plot at 5.5 x 5.5 inches, 300 dpi.
#'
#' @param design A `scf_mi_survey` object created by [scf_load()].
#' @param x A one-sided formula for the x-axis variable (e.g., `~income`).
#' @param y A one-sided formula for the y-axis variable (e.g., `~networth`).
#' @param bins Integer. Number of hexagonal bins along the x-axis. Default is `50`.
#' @param title Optional character string for the plot title.
#' @param xlab Optional x-axis label. Defaults to the variable name.
#' @param ylab Optional y-axis label. Defaults to the variable name.
#'
#' @return A `ggplot2` object displaying a Rubin-pooled hexbin plot.
#'
#' @section Dependencies:
#' Requires the `ggplot2` package. The fill scale uses `scale_fill_viridis_c()` from `ggplot2`.
#' Requires the \pkg{hexbin} package. The function will stop with an error if it is not installed.
#'
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td  <- tempdir()
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Plot hexbin of income vs. net worth
#' scf_plot_hex(scf2022, ~income, ~networth)
#' 
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(file.path(td, "scf2022.rds"), force = TRUE)
#'
#' @seealso [scf_corr()], [scf_plot_smooth()], [scf_theme()]
#'
#' @export
scf_plot_hex <- function(design, x, y,
                         bins = 50,
                         title = NULL,
                         xlab = NULL,
                         ylab = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Please install it.")
  }

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  if (isTRUE(attr(design, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }

  if (!inherits(x, "formula") || !inherits(y, "formula")) {
    stop("Both `x` and `y` must be one-sided formulas, e.g., ~income, ~networth")
  }

  if (!requireNamespace("hexbin", quietly = TRUE)) {
    stop("The 'hexbin' package is required for scf_plot_hex(). Please install it.")
  }

  xname <- as.character(x[[2]])
  yname <- as.character(y[[2]])

  data_list <- lapply(seq_along(design$mi_design), function(i) {
    d <- design$mi_design[[i]]
    df <- data.frame(
      x = d$variables[[xname]],
      y = d$variables[[yname]],
      wgt = as.numeric(weights(d)),
      imp = i
    )
    df
  })

  data_all <- do.call(rbind, data_list)

  data_all <- data_all[!is.na(data_all$x) & !is.na(data_all$y) & !is.na(data_all$wgt), ]

  ggplot2::ggplot(data_all, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hex(ggplot2::aes(weight = wgt), bins = bins) +
    ggplot2::scale_fill_viridis_c(trans = "log10", name = "Weighted Count") +
    ggplot2::labs(
      title = title %||% paste("Weighted Hexbin:", yname, "vs", xname),
      x = xlab %||% xname,
      y = ylab %||% yname
    ) +
    scf_theme()
}
