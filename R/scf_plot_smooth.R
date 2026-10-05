#' Smoothed Distribution Plot of a Continuous Variable in SCF Data
#'
#' Plots a smoothed curve of the share of households near each value.
#'
#' @details
#' Use it to see the shape, skew, or peaks of a variable such as age or
#' income. The curve shows shape, not exact shares. The implicates are
#' stacked with weights divided by five and binned (by default with the
#' Freedman-Diaconis width), and a smoothing curve is fit to the bin shares.
#'
#' @param design A `scf_mi_survey` object created by [scf_load()].
#' @param variable A one-sided formula specifying a continuous variable (e.g., `~networth`).
#' @param binwidth Optional bin width. Default uses Freedman-Diaconis rule.
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
#' @seealso [scf_plot_hist()], [scf_plot_dist()], [scf_theme()]
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   # Do not implement these lines in real analysis:
#'   # Use functions `scf_download()` and `scf_load()`
#'   td <- tempfile("plot_smooth_")
#'   dir.create(td)
#'
#'   src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#'   file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#'   scf2022 <- scf_load(2022, data_directory = td)
#'
#'   # Example for real analysis: Plot smoothed distribution
#'   scf_plot_smooth(
#'     scf2022,
#'     ~networth,
#'     xlim = c(0, 2e6),
#'     method = "loess",
#'     span = 0.25
#'   )
#'
#'   # Do not implement these lines in real analysis: Cleanup for package check
#'   unlink(td, recursive = TRUE, force = TRUE)
#' }
#' }
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

  varname <- deparse(variable[[2]])
  .scf_check_na(design$mi_design, variable)

  stacked <- lapply(seq_along(design$mi_design), function(i) {
    d <- design$mi_design[[i]]
    df <- d$variables
    data.frame(
      x = eval(variable[[2]], df, environment(variable)),
      wgt = as.numeric(weights(d, "sampling")) / length(design$mi_design),
      imp = i
    )
  })
  df <- do.call(rbind, stacked)
  df <- df[!is.na(df$x) & !is.na(df$wgt), ]

  if (!is.null(xlim)) {
    df <- df[df$x >= xlim[1] & df$x <= xlim[2], ]
  }
  if (nrow(df) == 0) stop("No valid data after subsetting.")

  if (is.null(binwidth)) {
    iqr <- IQR(df$x, na.rm = TRUE)
    n <- nrow(df)
    binwidth <- 2 * iqr / n^(1/3)
    if (!is.finite(binwidth) || binwidth <= 0) {
      binwidth <- diff(range(df$x, na.rm = TRUE)) / 50
    }
  }

  breaks <- seq(floor(min(df$x) / binwidth) * binwidth,
                ceiling(max(df$x) / binwidth) * binwidth,
                by = binwidth)
  mids <- head(breaks, -1) + binwidth / 2
  df$bin_index <- cut(df$x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  df$bin_center <- mids[df$bin_index]

  agg <- aggregate(wgt ~ bin_center, data = df, sum)
  names(agg)[1] <- "x"
  agg$percent <- 100 * agg$wgt / sum(agg$wgt)

  if (is.null(xlab)) xlab <- varname

  ggplot2::ggplot(agg, ggplot2::aes(x = x, y = percent)) +
    ggplot2::geom_smooth(
      method = method,
      span = if (method == "loess") span else NULL,
      se = FALSE,
      color = color
    ) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    scf_theme()
}
