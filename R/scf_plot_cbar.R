#' Bar Plot of Summary Statistics by Grouping Variable in SCF Data
#'
#' Plots a mean, median, or percentile of a continuous variable for each group.
#'
#' @details
#' Use it to show how a variable differs across groups, such as median net
#' worth by education. Bar heights are point estimates from [scf_mean()],
#' [scf_median()], or [scf_percentile()] with `by`; run those functions for
#' standard errors.
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
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("plot_cbar_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Visualize 90th percentile of income by education
#' scf_plot_cbar(scf2022, ~income, ~edcl, stat = 0.9, fill = "#D55E00")
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
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


  yname <- all.vars(yvar)[1]
  xname <- all.vars(xvar)[1]

  xvals <- design$mi_design[[1]]$variables[[xname]]
  if (is.numeric(xvals) && length(unique(xvals)) > 25) {
    stop("Grouping variable appears continuous. Please use a factor or discrete variable.")
  }

  results <- switch(
    as.character(stat),
    mean = scf_mean(design, yvar, by = xvar),
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

  df$group <- factor(df$group, levels = unique(df$group))

  if (!is.null(label_map)) {
    df$group <- .scf_relabel(df$group, label_map)
  }

  y_label <- ylab
  if (is.null(y_label)) y_label <- switch(
    as.character(stat),
    "mean" = paste("Mean of", yname),
    "median" = paste("Median of", yname),
    paste0(100 * stat, "th Percentile of ", yname)
  )

  if (is.null(title)) title <- paste("Distribution of", yname, "by", xname)
  if (is.null(xlab)) xlab <- xname

  ggplot2::ggplot(df, ggplot2::aes(x = group, y = estimate)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::scale_y_continuous(labels = .scf_comma) +
    ggplot2::labs(
      title = title,
      x = xlab,
      y = y_label
    ) +
    scf_theme() +
    ggplot2::theme(axis.text.x = .scf_axis_text_x(angle))
}
