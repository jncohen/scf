#' Hexbin Plot of Two Continuous SCF Variables
#'
#' Plots the joint distribution of two continuous variables with hexagonal
#' bins.
#'
#' @details
#' Use it in place of a scatterplot when there are too many points to see.
#' Brighter cells hold more households; the color scale is logarithmic. The
#' implicates are stacked with weights divided by five. Requires the
#' \pkg{hexbin} package.
#'
#' @param design A `scf_mi_survey` object from [scf_load()].
#' @param x A one-sided formula for the x-axis variable (e.g., `~income`).
#' @param y A one-sided formula for the y-axis variable (e.g., `~networth`).
#' @param bins Integer. Number of hexagonal bins in each direction. Default is `50`.
#' @param title Optional character string for the plot title.
#' @param xlab Optional x-axis label. Defaults to the variable name.
#' @param ylab Optional y-axis label. Defaults to the variable name.
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
#' # Plot hexbin of income vs. net worth
#' # Note: mock data has 200 rows per implicate; hexbin output will be sparse.
#' # Results on full SCF data will show a meaningful joint density.
#' if (requireNamespace("hexbin", quietly = TRUE)) {
#'   scf_plot_hex(scf2022, ~income, ~networth)
#' }
#' \dontshow{unlink(td, recursive = TRUE, force = TRUE)}
#'
#' @seealso [scf_corr()], [scf_plot_smooth()], [scf_theme()]
#'
#' @export
scf_plot_hex <- function(design, x, y,
                         bins = 50,
                         title = NULL,
                         xlab = NULL,
                         ylab = NULL) {

  if (!inherits(x, "formula") || !inherits(y, "formula")) {
    stop("Both `x` and `y` must be one-sided formulas, e.g., ~income, ~networth")
  }

  if (!requireNamespace("hexbin", quietly = TRUE)) {
    stop("The 'hexbin' package is required for scf_plot_hex(). Please install it.")
  }

  xname <- deparse(x[[2]])
  yname <- deparse(y[[2]])
  .scf_check_na(design$mi_design, x, y)

  data_list <- lapply(seq_along(design$mi_design), function(i) {
    d <- design$mi_design[[i]]
    df <- data.frame(
      x = eval(x[[2]], d$variables, environment(x)),
      y = eval(y[[2]], d$variables, environment(y)),
      wgt = as.numeric(weights(d, "sampling")) / length(design$mi_design),
      imp = i
    )
    df
  })

  data_all <- do.call(rbind, data_list)

  data_all <- data_all[!is.na(data_all$x) & !is.na(data_all$y) & !is.na(data_all$wgt), ]

  if (is.null(title)) title <- paste("Weighted Hexbin:", yname, "vs", xname)
  if (is.null(xlab)) xlab <- xname
  if (is.null(ylab)) ylab <- yname

  ggplot2::ggplot(data_all, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_hex(ggplot2::aes(weight = wgt), bins = bins) +
    ggplot2::scale_fill_viridis_c(trans = "log10", name = "Weighted Count") +
    ggplot2::scale_x_continuous(labels = .scf_comma) +
    ggplot2::scale_y_continuous(labels = .scf_comma) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    scf_theme()
}
