#' Plot a Univariate Distribution of an SCF Variable
#'
#' @description
#' This function provides a unified plotting interface for visualizing the
#' distribution of a single variable from multiply-imputed SCF data. Discrete
#' variables produce bar charts of pooled proportions; continuous variables
#' produce binned histograms.  Use this function to visualize the univariate
#' distribution of an SCF variable.
#'
#' @section Implementation:
#' For discrete variables (factor or numeric with <= 25 unique values), the
#' function uses [scf_freq()] to calculate category proportions and produces a
#' bar chart.  For continuous variables, it bins values across implicates and
#' estimates Rubin-pooled frequencies for each bin.
#'
#' Users may supply a named vector of custom axis labels using the `labels` argument.
#'
#' @param design A `scf_mi_survey` object created by [scf_load()].
#' @param variable A one-sided formula specifying the variable to plot.
#' @param bins Number of bins for continuous variables. Default is 30.
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
#' # Mock workflow for CRAN (demo only — not real SCF data)
#' td  <- tempdir()
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' scf_plot_dist(scf2022, ~own)
#' scf_plot_dist(scf2022, ~age, bins = 10)
#' 
#' unlink("scf2022.rds", force = TRUE)
#'
#' \donttest{
#' # Real workflow
#' scf_download(2022)
#' scf2022 <- scf_load(2022)
#' scf_plot_dist(scf2022, ~own)
#' scf_plot_dist(scf2022, ~age, bins = 10)
#' 
#' # Clean up
#' unlink("scf2022.rds", force = TRUE)
#' }
#'
#' @export
scf_plot_dist <- function(design, variable, bins = 30,
                          title = NULL, xlab = NULL, ylab = "Percent",
                          angle = 30, fill = "#0072B2", labels = NULL) {

  stopifnot(inherits(design, "scf_mi_survey"))
  stopifnot(inherits(variable, "formula"))

  if (isTRUE(attr(design, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }


  `%||%` <- function(a, b) if (!is.null(a)) a else b
  varname <- all.vars(variable)[1]
  xlab <- xlab %||% varname
  title <- title %||% paste("Distribution of", varname)

  values <- design$mi_design[[1]]$variables[[varname]]

  n_distinct <- function(x) length(unique(x[!is.na(x)]))
  is_discrete <- is.factor(values) || (is.numeric(values) && n_distinct(values) <= 25)

  if (is_discrete) {
    freq <- scf_freq(design, variable, percent = TRUE)
    df <- freq$results
    df$yval <- df$proportion
    df$xval <- factor(df$category, levels = unique(df$category))


    # Apply custom labels if supplied
    if (!is.null(labels)) {
      df$xval <- factor(as.character(df$xval),
                        levels = names(labels),
                        labels = unname(labels))
    }


  } else {
    # Continuous: bin and pool
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
      qbar <- mean(df$prop)
      ubar <- mean(df$var)
      b <- var(df$prop)
      m <- length(df$implicate)
      se <- sqrt(ubar + (1 + 1/m) * b)
      data.frame(xval = df$bin[1], yval = 100 * qbar, se = 100 * se)
    })
    df <- do.call(rbind, pooled)
  }

  ggplot2::ggplot(df, ggplot2::aes(x = xval, y = yval)) +
    ggplot2::geom_col(fill = fill) +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    scf_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle, hjust = 1))
}
