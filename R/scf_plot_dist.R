#' Plot a Univariate Distribution of an SCF Variable
#'
#' Plots the percent of households in each category or bin of a variable.
#'
#' @details
#' Use it for a quick look at any variable. Factors and numeric variables
#' with 25 or fewer values are treated as discrete and plotted with
#' [scf_freq()]. Continuous variables are binned with the same breaks in every
#' implicate and pooled.
#'
#' @param design A `scf_mi_survey` object created by [scf_load()].
#' @param variable A one-sided formula specifying the variable to plot.
#' @param bins Approximate number of bins for continuous variables; the
#'   breaks are set by [pretty()]. Default is 30.
#' @param title Optional plot title.
#' @param xlab Optional x-axis label.
#' @param ylab Optional y-axis label. Default is "Percent".
#' @param angle Angle for x-axis tick labels. Default is 30.
#' @param fill Fill color for bars. Default is `"#0072B2"`.
#' @param labels Optional named vector of custom axis labels (for discrete variables only).
#'
#' @return A `ggplot2` object.
#'
#' @seealso [scf_theme()]
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("plot_dist_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Distribution of homeownership
#' scf_plot_dist(scf2022, ~own)
#'
#' # Example for real analysis: Distribution of age
#' scf_plot_dist(scf2022, ~age, bins = 10)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @export
scf_plot_dist <- function(design, variable, bins = 30,
                          title = NULL, xlab = NULL, ylab = "Percent",
                          angle = 30, fill = "#0072B2", labels = NULL) {

  stopifnot(inherits(design, "scf_mi_survey"))
  stopifnot(inherits(variable, "formula"))

  varname <- all.vars(variable)[1]
  .scf_check_na(design$mi_design, variable)
  if (is.null(xlab)) xlab <- varname
  if (is.null(title)) title <- paste("Distribution of", varname)

  values <- design$mi_design[[1]]$variables[[varname]]

  n_distinct <- function(x) length(unique(x[!is.na(x)]))
  is_discrete <- is.factor(values) || is.character(values) || is.logical(values) ||
    (is.numeric(values) && n_distinct(values) <= 25)

  if (is_discrete) {
    freq <- scf_freq(design, variable, percent = TRUE)
    df <- freq$results
    df$yval <- df$proportion
    df$xval <- factor(df$category, levels = unique(df$category))

    if (!is.null(labels)) {
      df$xval <- .scf_relabel(df$xval, labels)
    }

  } else {
    all_vals <- unlist(lapply(design$mi_design, function(d) d$variables[[varname]]))
    rng <- range(all_vals, na.rm = TRUE)
    cutpoints <- pretty(rng, bins)
    labels_seq <- paste(head(cutpoints, -1), tail(cutpoints, -1), sep = "-")

    binned <- lapply(seq_along(design$mi_design), function(i) {
      d <- design$mi_design[[i]]
      x <- d$variables[[varname]]
      bin <- cut(x, breaks = cutpoints, include.lowest = TRUE, labels = labels_seq)
      d$variables$bin <- bin
      est <- survey::svymean(~bin, d)
      data.frame(bin = labels_seq,
                 prop = as.numeric(est),
                 var = diag(vcov(est)),
                 implicate = i,
                 stringsAsFactors = FALSE)
    })

    long <- do.call(rbind, binned)
    pooled <- lapply(split(long, long$bin), function(df) {
      df <- df[order(df$implicate), ]
      pooled_b <- .scf_pool(df$prop, df$var, .scf_variance_method(NULL))
      qbar <- pooled_b$estimate
      se <- pooled_b$se
      data.frame(xval = df$bin[1], yval = 100 * qbar, se = 100 * se)
    })
    df <- do.call(rbind, pooled)
    df$xval <- factor(df$xval, levels = labels_seq)
  }

  ggplot2::ggplot(df, ggplot2::aes(x = xval, y = yval)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    scf_theme() +
    ggplot2::theme(axis.text.x = .scf_axis_text_x(angle))
}
