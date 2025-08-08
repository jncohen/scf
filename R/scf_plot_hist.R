#' Histogram of a Continuous Variable in Multiply-Imputed SCF Data
#'
#' @description
#' Produces a histogram of a continuous SCF variable by binning across implicates,
#' pooling weighted bin counts using [scf_freq()], and plotting the result.
#' Values outside `xlim` are clamped into the nearest endpoint to ensure all
#' observations are included and replicate-weighted bins remain stable.
#'
#' @section Implementation:
#' This function bins a continuous variable (after clamping to `xlim` if supplied),
#' applies the same `cut()` breaks across implicates using [scf_update_by_implicate()],
#' and computes Rubin-pooled frequencies with [scf_freq()]. Results are filtered to
#' remove bins with undefined proportions and then plotted using `ggplot2::geom_col()`.
#'
#' The logic here is specific to operations where the bin assignment must be computed
#' **within** each implicate, not after pooling. This approach ensures consistent binning
#' and stable pooled estimation in the presence of multiply-imputed microdata.
#'
#' @param design A `scf_mi_survey` object from [scf_load()].
#' @param variable A one-sided formula indicating the numeric variable to plot.
#' @param bins Number of bins (default: 30).
#' @param xlim Optional numeric range. Values outside will be included in edge bins.
#' @param title Optional plot title.
#' @param xlab Optional x-axis label. Defaults to the variable name.
#' @param ylab Optional y-axis label. Defaults to "Weighted Count".
#' @param fill Fill color for bars (default: `"#0072B2"`).
#'
#' @return A `ggplot2` object representing the Rubin-pooled histogram.
#'
#' @examples
#' \donttest{
#' scf2022 <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))
#' scf_plot_hist(scf2022, ~age, xlim = c(20, 80))
#' scf_plot_hist(scf2022, ~income, bins = 40)
#' }
#'
#' @seealso [scf_freq()], [scf_plot_dbar()], [scf_plot_smooth()], [scf_update_by_implicate()]
#' @export
scf_plot_hist <- function(design, variable,
                          bins = 30,
                          xlim = NULL,
                          title = NULL,
                          xlab = NULL,
                          ylab = "Weighted Count",
                          fill = "#0072B2") {
  stopifnot(inherits(design, "scf_mi_survey"))
  
  if (isTRUE(attr(design, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }
  
  varname <- all.vars(variable)[1]
  
  # Extract pooled nonmissing values to define range
  all_values <- unlist(lapply(design$implicates, function(df) df[[varname]]))
  all_values <- all_values[is.finite(all_values)]
  if (length(all_values) == 0L) stop("No usable values found for this variable.")
  
  rng <- if (!is.null(xlim)) xlim else range(all_values, na.rm = TRUE)
  breaks <- seq(rng[1], rng[2], length.out = bins + 1)
  
  # Clamp variable to xlim and bin
  design <- scf_update_by_implicate(design, function(df) {
    clamped <- pmin(pmax(df[[varname]], rng[1]), rng[2])
    df$.binvar <- cut(clamped, breaks = breaks, include.lowest = TRUE)
    df
  })
  
  # Compute weighted frequencies
  freq <- scf_freq(design, ~.binvar, percent = FALSE)
  
  # Filter out NA or zero-mass bins
  results <- freq$results[is.finite(freq$results$proportion) & freq$results$proportion > 0, ]
  
  # Plot histogram
  ggplot(results, aes(x = category, y = proportion)) +
    geom_col(fill = fill) +
    labs(
      title = title %||% paste("Histogram of", varname),
      x = xlab %||% varname,
      y = ylab
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
