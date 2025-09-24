#' Format and Display Regression Results from Multiply-Imputed SCF Models
#'
#' This function formats and aligns coefficient estimates, standard errors, and
#' significance stars from one or more SCF regression model objects
#' (e.g., from \code{scf_ols()}, \code{scf_logit()}, or \code{scf_glm()}). 
#'
#' It compiles a side-by-side table with terms matched across models, appends
#' model fit statistics (sample size N, R-squared or pseudo-R-squared, and AIC),
#' and outputs the results as console text, Markdown for R Markdown documents,
#' or a CSV file.
#'
#' @param ... One or more SCF regression model objects, or a single list of such models.
#' @param model.names Optional character vector naming the models. Defaults to
#'   \code{"Model 1"}, \code{"Model 2"}, etc.
#' @param digits Integer specifying decimal places for numeric formatting when
#'   \code{auto_digits = FALSE}. Default is 0.
#' @param auto_digits Logical; if \code{TRUE}, uses adaptive decimal places:
#'   0 digits for large numbers (>= 1000), 2 digits for moderate (>= 1),
#'   and 3 digits for smaller values.
#' @param labels Optional named character vector or labeling function to replace
#'   term names with descriptive labels.
#' @param output Output format: one of \code{"console"} (print to console),
#'   \code{"markdown"} (print Markdown table for R Markdown), or \code{"csv"}
#'   (write CSV file).
#' @param file File path for CSV output; required if \code{output = "csv"}.
#'
#' @return Invisibly returns a data frame with formatted regression results and fit statistics.
#'
#' @details
#' The function aligns all unique coefficient terms across provided models, formats
#' coefficients with significance stars and standard errors, appends model fit
#' statistics as additional rows, and renders output in the specified format.
#' It avoids external dependencies by using base R formatting and simple text or
#' Markdown output.
#'
#' @examples
#' # Mock workflow for CRAN (demo only — not real SCF data)
#' td  <- tempdir()
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#' m1 <- scf_ols(scf2022, income ~ age)
#'
#' # Print regression results as a console table
#' scf_regtable(m1, digits = 2)
#' 
#' unlink("scf2022.rds", force = TRUE)
#'
#' \donttest{
#' # Real workflow
#' scf_download(2022)
#' scf2022 <- scf_load(2022)
#' m1 <- scf_ols(scf2022, income ~ age)
#' scf_regtable(m1, digits = 2)
#' 
#' # Clean up
#' unlink("scf2022.rds", force = TRUE)
#' }
#'
#' @export
scf_regtable <- function(...,
                         model.names = NULL,
                         digits = 0,
                         auto_digits = FALSE,
                         labels = NULL,
                         output = c("console", "markdown", "csv"),
                         file = NULL) {

  models <- list(...)

  # If the user passed a single argument that is a list of models, unpack it
  if (length(models) == 1 && is.list(models[[1]]) &&
      (inherits(models[[1]][[1]], "scf_ols") || inherits(models[[1]][[1]], "scf_logit") || inherits(models[[1]][[1]], "scf_glm"))) {
    models <- models[[1]]
  }

  output <- match.arg(output)
  n_models <- length(models)

  if (is.null(model.names)) model.names <- paste("Model", seq_len(n_models))
  stopifnot(length(model.names) == n_models)

  # Gather all terms from all models
  all_terms <- unique(unlist(lapply(models, function(m) m$results$term)))
  all_terms <- sort(all_terms)
  out <- data.frame(term = all_terms, stringsAsFactors = FALSE)

  # Helper: format estimates with adaptive digits or fixed digits
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
        } else if (absx > 0) {
          formatC(x, format = "f", digits = 3)
        } else {
          "0"
        }
      }, USE.NAMES = FALSE)
    }
  }

  # Fill main regression results into the output data.frame
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

  # Apply custom labels if provided
  if (!is.null(labels)) {
    if (is.function(labels)) {
      out$Term <- labels(out$Term)
    } else if (is.character(labels)) {
      out$Term <- sapply(out$Term, function(t) ifelse(!is.na(labels[t]), labels[t], t))
    }
  }

  # Prepare fit statistics rows: N, R2 or pseudo-R2, AIC
  fit_terms <- c("N", "R2", "AIC")
  fit_stats_mat <- matrix("--", nrow = length(fit_terms), ncol = n_models,
                          dimnames = list(fit_terms, model.names))

  for (i in seq_along(models)) {
    m <- models[[i]]

    # Extract sample size N from implicate or model residuals
    n <- NA_integer_
    if (!is.null(m$models) && length(m$models) > 0 && !is.null(m$models[[1]])) {
      n <- length(stats::residuals(m$models[[1]]))
    } else if (!is.null(m$imps) && length(m$imps) > 0 && !is.null(m$imps[[1]])) {
      n <- length(stats::residuals(m$imps[[1]]))
    }

    # Detect binomial/logit family for pseudo-R2 usage
    is_binomial <- FALSE
    if (!is.null(m$family)) {
      is_binomial <- inherits(m$family, "binomial")
    }
    if (!is_binomial && !is.null(m$models) && length(m$models) > 0) {
      is_binomial <- inherits(m$models[[1]], "glm") &&
        family(m$models[[1]])$family == "binomial"
    }

    # Safely get R2 or pseudo_R2; avoid NULL by assigning NA_real_ if needed
    r2_val <- NA_real_
    if (inherits(m, "scf_logit") || is_binomial) {
      r2_val <- if (!is.null(m$fit$pseudo_r2)) m$fit$pseudo_r2 else NA_real_
    } else {
      r2_val <- if (!is.null(m$fit$r.squared)) m$fit$r.squared else NA_real_
    }

    aic_val <- if (!is.null(m$fit$AIC)) m$fit$AIC else NA_real_

    fit_stats_mat["N", i] <- if (!is.na(n)) as.character(n) else "--"
    fit_stats_mat["R2", i] <- if (!is.na(r2_val)) formatC(r2_val, digits = 3, format = "f") else "--"
    fit_stats_mat["AIC", i] <- if (!is.na(aic_val)) formatC(aic_val, digits = 0, format = "f") else "--"
  }

  fit_stats_df <- data.frame(Term = fit_terms, fit_stats_mat, stringsAsFactors = FALSE)

  # Convert all columns to character to safely bind
  out[] <- lapply(out, as.character)
  fit_stats_df[] <- lapply(fit_stats_df, as.character)

  # Align column names for binding
  colnames(fit_stats_df) <- colnames(out)

  # Append fit stats below main table
  out <- rbind(out, fit_stats_df)

  # Output depending on user choice
  if (output == "console") {
    max_len <- max(nchar(out$Term))
    for (i in seq_len(nrow(out))) {
      cat(sprintf(paste0("%-", max_len, "s"), out$Term[i]))
      for (j in 2:ncol(out)) {
        cat("  ", format(out[i, j], justify = "right"))
      }
      cat("\n")
    }
    invisible(out)

  } else if (output == "csv") {
    if (is.null(file)) stop("Please provide a file path for CSV output.")
    write.csv(out, file = file, row.names = FALSE)
    invisible(out)

  } else if (output == "markdown") {
    header <- paste0("| ", paste(colnames(out), collapse = " | "), " |")
    separator <- paste0("|", paste(rep("---", ncol(out)), collapse = "|"), "|")
    rows <- apply(out, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
    md_table <- paste(c(header, separator, rows), collapse = "\n")
    cat(md_table, "\n")
    invisible(md_table)
  }
}
