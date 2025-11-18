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
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td  <- tempdir()
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Plot histogram of age
#' scf_plot_hist(scf2022, ~age, bins = 10)
#' 
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(file.path(td, "scf2022.rds"), force = TRUE)
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
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  varname <- all.vars(variable)[1]
  
  # Pull values from mi_design (not implicates)
  all_values <- unlist(lapply(design$mi_design, function(d) d$variables[[varname]]), use.names = FALSE)
  all_values <- all_values[is.finite(all_values)]
  if (length(all_values) == 0L) stop("No usable values found for this variable.")
  
  rng <- if (!is.null(xlim)) xlim else range(all_values, na.rm = TRUE)
  breaks <- seq(rng[1], rng[2], length.out = bins + 1)
  
  # Clamp and bin within each implicate
  design <- scf_update_by_implicate(design, function(df) {
    x <- df[[varname]]
    x <- pmin(pmax(x, rng[1]), rng[2])
    df$.binvar <- cut(x, breaks = breaks, include.lowest = TRUE, right = TRUE)
    df
  })
  
  # Weighted frequencies across bins
  freq <- scf_freq(design, ~.binvar, percent = FALSE)
  
  # Keep non-empty bins
  res <- freq$results
  res <- res[is.finite(res$proportion) & res$proportion > 0, , drop = FALSE]
  
  ggplot2::ggplot(res, ggplot2::aes(x = .data$category, y = .data$proportion)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::labs(
      title = if (is.null(title)) paste("Histogram of", varname) else title,
      x     = if (is.null(xlab))  varname                      else xlab,
      y     = ylab
    ) +
    scf_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}