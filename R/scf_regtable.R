#' Format and Display Regression Results from Multiply-Imputed SCF Models
#'
#' Formats one or more `scf` model results as a side-by-side table.
#'
#' @details
#' Use it to report several models together. Each cell shows the estimate,
#' significance stars, and standard error in parentheses. Terms appear in the
#' order first seen; terms absent from a model show "--". Fit statistics
#' follow the coefficients: N for all models, R-squared and AIC for OLS, AIC
#' and pseudo-R-squared for binomial models, and tau, R1, and adjusted R1 for
#' quantile regression. Compare AIC only across models of the same type on the
#' same data.
#'
#' @param ... One or more SCF regression model objects, or a single list of such models.
#' @param model.names Optional character vector naming the models. Defaults to
#'   `"Model 1"`, `"Model 2"`, etc.
#' @param digits Integer. Decimal places for every estimate. If NULL
#'   (default), decimal places are chosen by size (see `auto_digits`).
#' @param auto_digits Logical; if `TRUE`, uses adaptive decimal places:
#'   0 digits for large numbers (>= 1000), 2 digits for moderate (>= 1),
#'   3 digits for values of at least 0.001, and 2 significant digits below that.
#' @param labels Optional named character vector or labeling function to replace
#'   term names with descriptive labels.
#' @param output Output format: one of `"console"` (print to console),
#'   `"markdown"` (print Markdown table for R Markdown), `"latex"`
#'   (print LaTeX table for PDF compilation), or `"csv"`
#'   (write CSV file).
#' @param file File path for CSV output; required if `output = "csv"`.
#'
#' @return Invisibly returns a data frame with formatted regression results and fit statistics.
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("regtable_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Wrangle data for example:  Perform OLS regression
#' m1 <- scf_ols(scf2022, income ~ age)
#'
#' # Example for real analysis: Print regression results as a console table
#' scf_regtable(m1, digits = 2)
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @export
scf_regtable <- function(...,
                         model.names = NULL,
                         digits = NULL,
                         auto_digits = is.null(digits),
                         labels = NULL,
                         output = c("console", "markdown", "latex", "csv"),
                         file = NULL) {

  models <- list(...)

  if (length(models) == 1 && is.list(models[[1]]) &&
      (inherits(models[[1]][[1]], "scf_model_result"))) {
    models <- models[[1]]
  }

  output <- match.arg(output)
  n_models <- length(models)

  if (is.null(model.names)) model.names <- paste("Model", seq_len(n_models))
  stopifnot(length(model.names) == n_models)

  all_terms <- unique(unlist(lapply(models, function(m) m$results$term)))
  out <- data.frame(term = all_terms, stringsAsFactors = FALSE)

  format_estimate <- function(est) {
    if (!auto_digits) {
      formatC(est, format = "f", digits = digits)
    } else {
      sapply(est, function(x) {
        absx <- abs(x)
        if (absx >= 1000) {
          formatC(x, format = "f", digits = 0)
        } else if (absx >= 1) {
          formatC(x, format = "f", digits = 2)
        } else if (absx >= 0.001) {
          formatC(x, format = "f", digits = 3)
        } else if (absx > 0) {
          formatC(x, format = "g", digits = 2)
        } else {
          "0"
        }
      }, USE.NAMES = FALSE)
    }
  }

  for (i in seq_along(models)) {
    res <- models[[i]]$results
    est_str <- format_estimate(res$estimate)
    se_str <- format_estimate(res$std.error)
    stars <- as.character(res$stars)
    formatted <- paste0(est_str, stars, " (", se_str, ")")
    vals <- rep("--", length(all_terms))
    names(vals) <- all_terms
    vals[res$term] <- formatted
    out[[model.names[i]]] <- vals
  }

  colnames(out)[1] <- "Term"

  if (!is.null(labels)) {
    if (is.function(labels)) {
      out$Term <- labels(out$Term)
    } else if (is.character(labels)) {
      out$Term <- sapply(out$Term, function(t) ifelse(!is.na(labels[t]), labels[t], t))
    }
  }

  is_quantreg <- sapply(models, function(m) inherits(m, "scf_quantreg"))
  is_binomial <- sapply(models, function(m) {
    if (inherits(m, "scf_logit")) return(TRUE)
    imps <- if (!is.null(m$models)) m$models else m$imps
    length(imps) > 0 && inherits(imps[[1]], "glm") &&
      identical(stats::family(imps[[1]])$family, "binomial")
  })

  fit_terms <- c("N",
                 if (any(!is_quantreg & !is_binomial)) "R2",
                 if (any(is_binomial)) "PseudoR2",
                 if (any(is_quantreg)) c("Tau", "R1", "R1(adj)"),
                 if (any(!is_quantreg)) "AIC")

  fit_stats_mat <- matrix("--", nrow = length(fit_terms), ncol = n_models,
                          dimnames = list(fit_terms, model.names))

  fmt <- function(x, d) if (is.null(x) || is.na(x)) "--" else formatC(x, digits = d, format = "f")

  for (i in seq_along(models)) {
    m <- models[[i]]

    n <- if (!is.null(m$fit$nobs_mean)) m$fit$nobs_mean else
      tryCatch(length(stats::residuals(m)), error = function(e) NA_real_)
    fit_stats_mat["N", i] <- fmt(n, if (isTRUE(n == round(n))) 0 else 1)

    if (is_quantreg[i]) {
      fit_stats_mat["Tau", i] <- fmt(m$tau, 2)
      fit_stats_mat["R1", i] <- fmt(m$fit$r1, 3)
      fit_stats_mat["R1(adj)", i] <- fmt(m$fit$r1_adj, 3)
    } else if (is_binomial[i]) {
      fit_stats_mat["PseudoR2", i] <- fmt(m$fit$pseudo_r2, 3)
    } else {
      fit_stats_mat["R2", i] <- fmt(m$fit$r.squared, 3)
    }

    if (!is_quantreg[i]) fit_stats_mat["AIC", i] <- fmt(m$fit$AIC, 0)
  }

  fit_stats_df <- data.frame(Term = fit_terms, fit_stats_mat, stringsAsFactors = FALSE)

  out[] <- lapply(out, as.character)
  fit_stats_df[] <- lapply(fit_stats_df, as.character)

  colnames(fit_stats_df) <- colnames(out)

  out <- rbind(out, fit_stats_df)

  if (output == "console") {
    widths <- sapply(names(out), function(nm) max(nchar(c(nm, out[[nm]]))))
    pad <- function(x, w, left) formatC(x, width = w, flag = if (left) "-" else " ")
    line <- function(vals) {
      cells <- mapply(pad, vals, widths, c(TRUE, rep(FALSE, length(vals) - 1)))
      cat(paste(cells, collapse = "  "), "\n", sep = "")
    }
    n_coef <- nrow(out) - length(fit_terms)
    line(names(out))
    for (i in seq_len(nrow(out))) {
      if (i == n_coef + 1) cat(strrep("-", sum(widths) + 2 * (length(widths) - 1)), "\n", sep = "")
      line(unlist(out[i, ]))
    }
    invisible(out)

  } else if (output == "csv") {
    if (is.null(file)) stop("Please provide a file path for CSV output.")
    write.csv(out, file = file, row.names = FALSE)
    invisible(out)

  } else if (output == "markdown") {
    out_escaped <- out
    out_escaped$Term <- gsub("_", "\\_", out_escaped$Term, fixed = TRUE)
    out_escaped[] <- lapply(out_escaped, function(col) {
      gsub("^", "\\^", col, fixed = TRUE)
    })

    header <- paste0("| ", paste(colnames(out_escaped), collapse = " | "), " |")
    separator <- paste0("|", paste(rep("---", ncol(out_escaped)), collapse = "|"), "|")
    rows <- apply(out_escaped, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
    md_table <- paste(c(header, separator, rows), collapse = "\n")
    cat(md_table, "\n")
    invisible(out)

  } else if (output == "latex") {
    latex_escape <- function(x) {
      x <- gsub("([&%$#_])", "\\\\\\1", x)
      gsub("^", "\\^{}", x, fixed = TRUE)
    }

    cat("\\begin{table}\n")
    cat("\\centering\n")
    cat("\\begin{tabular}{l", paste(rep("r", ncol(out) - 1), collapse = ""), "}\n", sep = "")
    cat("\\toprule\n")

    header <- paste(latex_escape(colnames(out)), collapse = " & ")
    cat(header, " \\\\\n")
    cat("\\midrule\n")

    n_coef <- nrow(out) - length(fit_terms)
    for (i in seq_len(nrow(out))) {
      row_str <- paste(latex_escape(unlist(out[i, ])), collapse = " & ")
      cat(row_str, " \\\\\n")
      if (i == n_coef) cat("\\midrule\n")
    }

    cat("\\bottomrule\n")
    cat("\\end{tabular}\n")
    cat("\\end{table}\n")
    invisible(out)
  }
}
