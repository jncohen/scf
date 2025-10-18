#' Estimate Mean in Multiply-Imputed SCF Data
#'
#' @description
#' Returns the population-level estimate of a continuous variable's weighted
#' mean across the Survey's five implicates. Use this operation to derive an
#' estimate of a population's 'typical' or 'average' score on a continuous
#' variable.
#'
#' @section Details:
#' The mean is a measure of central tendency that represents the arithmetic average of a distribution.
#' It is most appropriate when the distribution is symmetric and not heavily skewed.
#' Unlike the median, the mean is sensitive to extreme values, which may distort interpretation in the presence of outliers.
#' Use this function to describe the “typical” value of a continuous variable in the population or within subgroups.
#'
#' @param scf A scf_mi_survey object created with [scf_load()]. Must contain five replicate-weighted implicates.
#' @param var A one-sided formula identifying the continuous variable to summarize (e.g., ~networth).
#' @param by Optional one-sided formula specifying a discrete grouping variable for stratified means.
#' @param verbose Logical. If TRUE, include implicate-level results in print output. Default is FALSE.
#'
#' @return A list of class "scf_mean" with:
#' \describe{
#'   \item{results}{Pooled estimates with standard errors and range across implicates. One row per group, or one row total.}
#'   \item{imps}{A named list of implicate-level estimates.}
#'   \item{aux}{Variable and group metadata.} 
#' }
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td  <- tempdir()
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Estimate means
#' scf_mean(scf2022, ~networth)
#' scf_mean(scf2022, ~networth, by = ~edcl)
#' 
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink("scf2022.rds", force = TRUE)
#'
#'
#' @seealso [scf_median()], [scf_percentile()], [scf_xtab()], [scf_plot_dist()]
#'
#' @export
scf_mean <- function(scf, var, by = NULL, verbose = FALSE) {

  # Ensure data object is a well-formed scf_mi_survey object
  # with a list of valid `survey` replicate designs
  if (!inherits(scf, "scf_mi_survey") ||
      !is.list(scf$mi_design) ||
      !all(sapply(scf$mi_design, inherits, "svyrep.design"))) {
    stop("Input must be a 'scf_mi_survey' object with replicate-weighted implicates.")
  }

  if (isTRUE(attr(scf, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }


  # Focal and grouping variable names assigned to within-function objects
  varname <- all.vars(var)[1]
  byname <- if (!is.null(by)) all.vars(by)[1] else NULL
  designs <- scf$mi_design
  nimp <- length(designs)

  # For each implicate, compute replicate-weighted means using
  # `svymean()` or `svyby()` as appropriate. Store results as a list
  # of data frames (one per implicate).
  imps <- lapply(seq_len(nimp), function(i) {
    d <- designs[[i]]
    if (!is.null(byname)) {
      d$variables[[byname]] <- factor(d$variables[[byname]])
      est <- survey::svyby(var, by, survey::svymean, design = d)
      data.frame(
        implicate = i,
        group = est[[byname]],
        estimate = as.numeric(est[[varname]]),
        stringsAsFactors = FALSE
      )
    } else {
      est <- survey::svymean(var, d)
      data.frame(
        implicate = i,
        estimate = as.numeric(est),
        stringsAsFactors = FALSE
      )
    }
  })
  names(imps) <- paste0("imp", seq_len(nimp))

  # Combine implicate-level results into a single data frame
  df <- do.call(rbind, imps)

  # Calculate the pooled mean and standard error
  if (is.null(byname)) {
    x <- df$estimate
    qbar <- mean(x)
    b <- var(x)
    se <- sqrt((1 + 1 / nimp) * b)
    out <- data.frame(
      variable = varname,
      estimate = qbar,
      se = se,
      min = min(x),
      max = max(x),
      stringsAsFactors = FALSE
    )
  } else {
    group_levels <- sort(unique(df$group))
    out <- do.call(rbind, lapply(group_levels, function(g) {
      x <- df$estimate[df$group == g]
      qbar <- mean(x)
      b <- var(x)
      se <- sqrt((1 + 1 / nimp) * b)
      data.frame(
        group = g,
        variable = varname,
        estimate = qbar,
        se = se,
        min = min(x),
        max = max(x),
        stringsAsFactors = FALSE
      )
    }))
    out <- out[, c("group", "variable", "estimate", "se", "min", "max")]
  }

  # Assemble results to return
  out_obj <- list(
    results = out,
    imps = imps,
    aux = list(varname = varname, byname = byname),
    verbose = verbose
  )
  class(out_obj) <- "scf_mean"
  return(out_obj)
}
#' @export
print.scf_mean <- function(x, ...) {
  cat("Multiply-Imputed, Replicate-Weighted Mean Estimate\n\n")
  print(x$results, row.names = FALSE, ...)

  if (isTRUE(x$verbose)) {
    cat("\nImplicate-Level Estimates:\n\n")
    imp_df <- do.call(rbind, x$imps)
    print(imp_df, row.names = FALSE)
  }

  invisible(x)
}
#' @export
summary.scf_mean <- function(object, ...) {
  cat("Summary of Multiply-Imputed Mean Estimate\n\n")

  # Print pooled estimates with standard errors
  cat("Pooled Estimates:\n")
  print(
    format(object$results, digits = 4, nsmall = 2),
    row.names = FALSE,
    ...
  )

  # Optionally print implicate-level estimates
  cat("\nImplicate-Level Estimates:\n")
  imp_df <- do.call(rbind, object$imps)
  print(
    format(imp_df, digits = 4, nsmall = 2),
    row.names = FALSE
  )

  invisible(object)
}
