#' Format and Display Regression Results from Multiply-Imputed SCF Models
#'
#' This function formats and aligns coefficient estimates, standard errors, and
#' significance stars from one or more SCF regression model objects
#' (e.g., from \code{scf_ols()}, \code{scf_logit()}, \code{scf_quantreg()}, or \code{scf_glm()}). 
#'
#' It compiles a side-by-side table with terms matched across models, appends
#' model fit statistics (sample size N, R-squared or pseudo-R-squared, quantile tau,
#' and AIC where applicable), and outputs the results as console text, Markdown for
#' R Markdown documents, LaTeX for PDF compilation, or a CSV file.
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
#'   \code{"markdown"} (print Markdown table for R Markdown), \code{"latex"}
#'   (print LaTeX table for PDF compilation), or \code{"csv"}
#'   (write CSV file).
#' @param file File path for CSV output; required if \code{output = "csv"}.
#'
#' @return Invisibly returns a data frame with formatted regression results and fit statistics.
#'
#' @details
#' The function aligns all unique coefficient terms across provided models, formats
#' coefficients with significance stars and standard errors, appends model fit
#' statistics as additional rows, and renders output in the specified format.
#'
#' Fit statistics rows are automatically selected based on model class:
#' \describe{
#'   \item{All models}{Sample size (N)}
#'   \item{OLS models}{R-squared}
#'   \item{Logit/GLM models}{Pseudo-R-squared}
#'   \item{Quantile regression}{Quantile tau}
#'   \item{OLS and Logit (not quantreg)}{AIC}
#' }
#'
#' It avoids external dependencies by using base R formatting and simple text,
#' Markdown, LaTeX, or CSV output.
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
                         digits = 0,
                         auto_digits = FALSE,
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
  
  # ========================================================================
  # DETERMINE FIT STATISTICS ROWS DYNAMICALLY
  # ========================================================================
  # Check if any model is quantile regression
  is_quantreg <- any(sapply(models, function(m) inherits(m, "scf_quantreg")))
  is_logit <- any(sapply(models, function(m) inherits(m, "scf_logit")))
  
  # Build fit terms list dynamically
  fit_terms <- c("N")
  if (!is_quantreg) {
    if (is_logit) {
      fit_terms <- c(fit_terms, "PseudoR2")
    } else {
      fit_terms <- c(fit_terms, "R2")
    }
    fit_terms <- c(fit_terms, "AIC")
  } else {
    # For quantreg, add tau column
    fit_terms <- c(fit_terms, "Tau")
  }
  
  fit_stats_mat <- matrix("--", nrow = length(fit_terms), ncol = n_models,
                          dimnames = list(fit_terms, model.names))
  
  for (i in seq_along(models)) {
    m <- models[[i]]
    
    # Fetch N
    n <- NA_integer_
    tryCatch({
      n <- length(stats::residuals(m)) 
    }, error = function(e) {
      # Fallback to old, manual logic if S3 method fails (for robustness)
      if (!is.null(m$models) && length(m$models) > 0 && !is.null(m$models[[1]])) {
        n <<- length(stats::residuals(m$models[[1]]))
      } else if (!is.null(m$imps) && length(m$imps) > 0 && !is.null(m$imps[[1]])) {
        n <<- length(stats::residuals(m$imps[[1]]))
      }
    })
    
    fit_stats_mat["N", i] <- if (!is.na(n)) as.character(n) else "--"
    
    # Quantile regression: add tau
    if (inherits(m, "scf_quantreg")) {
      tau_val <- if (!is.null(m$tau)) m$tau else NA_real_
      fit_stats_mat["Tau", i] <- if (!is.na(tau_val)) formatC(tau_val, digits = 2, format = "f") else "--"
    } else {
      # Standard models: add R2/PseudoR2 and AIC
      
      # Fetch AIC
      aic_val <- NA_real_
      tryCatch({
        aic_val <- stats::AIC(m)
      }, error = function(e) {
        # Fallback to old, manual logic
        aic_val <<- if (!is.null(m$fit$AIC)) m$fit$AIC else NA_real_
      })
      
      # Detect binomial/logit family for pseudo-R2 usage 
      is_binomial <- FALSE
      if (!is.null(m$family)) {
        is_binomial <- inherits(m$family, "binomial")
      }
      if (!is_binomial && !is.null(m$models) && length(m$models) > 0) {
        is_binomial <- inherits(m$models[[1]], "glm") &&
          stats::family(m$models[[1]])$family == "binomial"
      }
      
      # Get R2 or pseudo_R2 
      r2_val <- NA_real_
      r2_label <- "R2"
      if (inherits(m, "scf_logit") || is_binomial) {
        r2_val <- if (!is.null(m$fit$pseudo_r2)) m$fit$pseudo_r2 else NA_real_
        r2_label <- "PseudoR2"
      } else {
        r2_val <- if (!is.null(m$fit$r.squared)) m$fit$r.squared else NA_real_
      }
      
      fit_stats_mat[r2_label, i] <- if (!is.na(r2_val)) formatC(r2_val, digits = 3, format = "f") else "--"
      fit_stats_mat["AIC", i] <- if (!is.na(aic_val)) formatC(aic_val, digits = 0, format = "f") else "--"
    }
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
    
  } else if (output == "latex") {
    # LaTeX table format using booktabs for nice horizontal lines
    cat("\\begin{table}\n")
    cat("\\centering\n")
    cat("\\begin{tabular}{l", paste(rep("r", ncol(out) - 1), collapse = ""), "}\n", sep = "")
    cat("\\toprule\n")
    
    # Header row
    header <- paste(colnames(out), collapse = " & ")
    cat(header, " \\\\\n")
    cat("\\midrule\n")
    
    # Data rows - insert midrule before fit stats
    n_coef <- nrow(out) - length(fit_terms)
    for (i in seq_len(nrow(out))) {
      row_str <- paste(out[i, ], collapse = " & ")
      cat(row_str, " \\\\\n")
      if (i == n_coef) cat("\\midrule\n")
    }
    
    cat("\\bottomrule\n")
    cat("\\end{tabular}\n")
    cat("\\end{table}\n")
    invisible(out)
  }
}