#' Smoothed Distribution Plot of a Continuous Variable in SCF Data
#'
#' @description
#' Draws a smoothed distribution plot of a continuous variable in the SCF. Use
#' this function to visualize a single continuous variable's distribution. 
#'
#' @section Implementation:
#' Visualizes the weighted distribution of a continuous SCF variable by stacking implicates,
#' binning observations, and smoothing pooled proportions. This function is useful for
#' examining distribution shape, skew, or modality in variables like income or wealth.
#'
#' All implicates are stacked and weighted, binned across a data-driven or user-specified
#' bin width. Each bin's weight share is calculated, and a smoothing curve is fit to
#' the resulting pseudo-density.
#'
#' @param design A `scf_mi_survey` object created by [scf_load()].
#' @param variable A one-sided formula specifying a continuous variable (e.g., `~networth`).
#' @param binwidth Optional bin width. Default uses Freedman–Diaconis rule.
#' @param xlim Optional numeric vector of length 2 to truncate axis.
#' @param method Character. Smoothing method: `"loess"` (default) or `"lm"`.
#' @param span Numeric LOESS span. Default is `0.2`. Ignored if `method = "lm"`.
#' @param color Line color. Default is `"blue"`.
#' @param xlab Optional label for x-axis. Defaults to the variable name.
#' @param ylab Optional label for y-axis. Defaults to `"Percent of Households"`.
#' @param title Optional plot title.
#'
#' @return A `ggplot2` object.
#'
#' @seealso [scf_theme()], [scf_plot_dist()]
#'
#' @examples
#' # Load bundled mock data (for demonstration only — not real SCF data)
#' td  <- tempdir()
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Smoothed distribution of net worth
#' scf_plot_smooth(scf2022, ~networth, xlim = c(0, 2e6),
#'                 method = "loess", span = 0.25)
#'                 
#'  unlink("scf2022.rds", force = TRUE)
#'
#' \donttest{
#' # Real workflow
#' scf_download(2022)
#' scf2022 <- scf_load(2022)
#' scf_plot_smooth(scf2022, ~networth, xlim = c(0, 2e6))
#' 
#' # Clean up
#' unlink("scf2022.rds", force = TRUE)
#' }
#' 
#'
#' @export
scf_plot_smooth <- function(design,
                            variable,
                            binwidth = NULL,
                            xlim = NULL,
                            method = "loess",
                            span = 0.2,
                            color = "blue",
                            xlab = NULL,
                            ylab = "Percent of Households",
                            title = NULL) {

  stopifnot(inherits(design, "scf_mi_survey"))
  stopifnot(inherits(variable, "formula"))

  if (isTRUE(attr(design, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }


  `%||%` <- function(a, b) if (!is.null(a)) a else b
  varname <- as.character(variable[[2]])

  # Stack implicates
  stacked <- lapply(seq_along(design$mi_design), function(i) {
    d <- design$mi_design[[i]]
    df <- d$variables
    data.frame(
      x = df[[varname]],
      wgt = as.numeric(weights(d)),
      imp = i
    )
  })
  df <- do.call(rbind, stacked)
  df <- df[!is.na(df$x) & !is.na(df$wgt), ]

  if (!is.null(xlim)) {
    df <- df[df$x >= xlim[1] & df$x <= xlim[2], ]
  }
  if (nrow(df) == 0) stop("No valid data after subsetting.")

  # Binwidth estimation
  if (is.null(binwidth)) {
    iqr <- IQR(df$x, na.rm = TRUE)
    n <- nrow(df)
    binwidth <- 2 * iqr / n^(1/3)
    if (!is.finite(binwidth) || binwidth <= 0) {
      binwidth <- diff(range(df$x, na.rm = TRUE)) / 50
    }
  }

  # Create bins
  breaks <- seq(floor(min(df$x) / binwidth) * binwidth,
                ceiling(max(df$x) / binwidth) * binwidth,
                by = binwidth)
  mids <- head(breaks, -1) + binwidth / 2
  df$bin_index <- cut(df$x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  df$bin_center <- mids[df$bin_index]

  # Aggregate proportions
  agg <- aggregate(wgt ~ bin_center, data = df, sum)
  names(agg)[1] <- "x"
  agg$percent <- 100 * agg$wgt / sum(agg$wgt)

  # Plot
  ggplot2::ggplot(agg, ggplot2::aes(x = x, y = percent)) +
    ggplot2::geom_smooth(
      method = method,
      span = if (method == "loess") span else NULL,
      se = FALSE,
      color = color
    ) +
    ggplot2::labs(
      title = title,
      x = xlab %||% varname,
      y = ylab
    ) +
    scf_theme()
}
