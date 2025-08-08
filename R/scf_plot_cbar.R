#' Bar Plot of Summary Statistics by Grouping Variable in SCF Data
#'
#' @description
#' Computes and plots a grouped summary statistic (either a mean, median, or
#' quantile) for a continuous variable across a discrete factor. Estimates are
#' pooled across implicates using [scf_mean()], [scf_median()], or
#' [scf_percentile()]. Use this function to visualize the bivariate relationship
#' between a discrete and a continuous variable.
#'
#' @section Implementation:
#' The user specifies a continuous outcome (`yvar`) and a discrete grouping
#' variable (`xvar`) via one-sided formulas. Group means are plotted by default.
#' Medians or other percentiles can be specified via the `stat` argument.
#'
#' Results are plotted using `ggplot2::geom_col()`, styled with [scf_theme()],
#' and optionally customized with additional arguments (e.g., axis labels,
#' color, angles).
#'
#' @param design A `scf_mi_survey` object from [scf_load()].
#' @param yvar One-sided formula for the continuous variable (e.g., `~networth`).
#' @param xvar One-sided formula for the grouping variable (e.g., `~racecl`).
#' @param stat `"mean"` (default), `"median"`, or a quantile (numeric between 0 and 1).
#' @param title Plot title (optional).
#' @param xlab X-axis label (optional).
#' @param ylab Y-axis label (optional).
#' @param fill Bar fill color. Default is `"#0072B2"`.
#' @param angle Angle of x-axis labels. Default is 30.
#' @param label_map Optional named vector to relabel x-axis category labels.
#'
#' @return A `ggplot2` object.
#'
#' @seealso [scf_mean()], [scf_median()], [scf_percentile()], [scf_theme()]
#'
#' @examples
#' # Load SCF data
#' \donttest{
#' # Load bundled mock data (for demonstration only — not real SCF data)
#' scf2022 <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))

#' # For real analysis, use:
#' # scf_download(2022); scf2022 <- scf_load(2022)
#'
#' # Plot mean net worth by education level
#' scf_plot_cbar(scf2022, ~networth, ~edcl, stat = "mean")
#'
#' # Visualize 90th percentile of income by race
#' scf_plot_cbar(scf2022, ~income, ~racecl, stat = 0.9, fill = "#D55E00")
#' }
#'
#' @export
scf_plot_cbar <- function(design, yvar, xvar,
                          stat = "mean",
                          title = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          fill = "#0072B2",
                          angle = 30,
                          label_map = NULL) {

  stopifnot(inherits(design, "scf_mi_survey"))
  stopifnot(inherits(yvar, "formula"), inherits(xvar, "formula"))

  if (isTRUE(attr(design, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }


  `%||%` <- function(a, b) if (!is.null(a)) a else b

  yname <- all.vars(yvar)[1]
  xname <- all.vars(xvar)[1]

  # Validate that xvar is discrete
  xvals <- design$mi_design[[1]]$variables[[xname]]
  if (is.numeric(xvals) && length(unique(xvals)) > 25) {
    stop("Grouping variable appears continuous. Please use a factor or discrete variable.")
  }

  # Capture factor levels from first implicate
  x_levels <- if (is.factor(xvals)) levels(xvals) else unique(xvals)

  # Dispatch to appropriate estimator
  results <- switch(
    as.character(stat),
    mean   = scf_mean(design, yvar, by = xvar),
    median = scf_median(design, yvar, by = xvar),
    {
      if (is.numeric(stat) && stat > 0 && stat < 1) {
        scf_percentile(design, yvar, q = stat, by = xvar)
      } else {
        stop("`stat` must be 'mean', 'median', or a quantile between 0 and 1.")
      }
    }
  )

  df <- results$results
  if (!all(c("group", "estimate") %in% names(df))) {
    stop("Estimation failed: expected 'group' and 'estimate' columns not found.")
  }

  # Coerce to factor and preserve ordering
  df$group <- factor(df$group, levels = x_levels)

  # Optional x-axis relabeling
  if (!is.null(label_map)) {
    df$group <- factor(df$group,
                       levels = names(label_map),
                       labels = unname(label_map))
  }

  # Auto-generate y-axis label if not provided
  y_label <- ylab %||% switch(
    as.character(stat),
    "mean"   = paste("Mean of", yname),
    "median" = paste("Median of", yname),
    paste0(100 * stat, "th Percentile of ", yname)
  )

  # Construct ggplot
  ggplot2::ggplot(df, ggplot2::aes(x = group, y = estimate)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::labs(
      title = title %||% paste("Distribution of", yname, "by", xname),
      x = xlab %||% xname,
      y = y_label
    ) +
    scf_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle, hjust = 1))
}
