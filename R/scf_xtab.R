#' Cross-Tabulate Two Discrete Variables in Multiply-Imputed SCF Data
#'
#' @description
#' Computes replicate-weighted two-way cross-tabulations of two discrete variables
#' using multiply-imputed SCF data. Estimates cell proportions and standard errors,
#' with optional scaling of proportions by cell, row, or column. Results are pooled
#' across implicates using Rubin's Rules.
#'
#' @param scf A `scf_mi_survey` object, typically created by [scf_load()]. Must include five implicates with replicate weights.
#' @param rowvar A one-sided formula specifying the row variable (e.g., `~edcl`).
#' @param colvar A one-sided formula specifying the column variable (e.g., `~racecl`).
#' @param scale Character. Proportion basis: "cell" (default), "row", or "col".
#'
#' @return A list of class `"scf_xtab"` with:
#' \describe{
#'   \item{results}{Data frame with one row per cell. Columns: `row`, `col`, `prop`, `se`, `row_share`, `col_share`, `rowvar`, and `colvar`.}
#'   \item{matrices}{List of matrices: `cell` (default proportions), `row`, `col`, and `se`.}
#'   \item{imps}{List of implicate-level cell count tables.}
#'   \item{aux}{List with `rowvar` and `colvar` names.}
#' }
#'
#' @section Statistical Notes:
#' Implicate-level tables are created using `svytable()` on replicate-weighted designs.
#' Proportions are calculated as shares of total population estimates. Variance across
#' implicates is used to estimate uncertainty. Rubin's Rules are applied in simplified form.
#'
#' For technical details on pooling logic, see [scf_MIcombine()] or the SCF package manual.
#'
#' @examples
#' # Do not implement these lines in real analysis:
#' # Use functions `scf_download()` and `scf_load()`
#' td <- tempfile("xtab_")
#' dir.create(td)
#'
#' src <- system.file("extdata", "scf2022_mock_raw.rds", package = "scf")
#' file.copy(src, file.path(td, "scf2022.rds"), overwrite = TRUE)
#' scf2022 <- scf_load(2022, data_directory = td)
#'
#' # Example for real analysis: Cross-tabulate ownership by sex
#' if (interactive()) {
#'   suppressWarnings(scf_xtab(scf2022, ~own, ~hhsex, scale = "row"))
#' }
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @importFrom stats as.formula ave
#' @export
scf_xtab <- function(scf, rowvar, colvar, scale = "cell") {
  scale <- match.arg(scale, choices = c("cell", "row", "col"))
  
  if (!inherits(scf, "scf_mi_survey")) {
    stop("Input must be an 'scf_mi_survey' object.", call. = FALSE)
  }
  
  if (isTRUE(attr(scf, "mock"))) {
    warning(
      "Mock data detected. Do not interpret results as valid SCF estimates.",
      call. = FALSE
    )
  }
  
  row_vars <- all.vars(rowvar)
  col_vars <- all.vars(colvar)
  
  if (length(row_vars) != 1L) {
    stop("`rowvar` must be a one-sided formula naming exactly one variable.", call. = FALSE)
  }
  
  if (length(col_vars) != 1L) {
    stop("`colvar` must be a one-sided formula naming exactly one variable.", call. = FALSE)
  }
  
  rowname <- row_vars[1]
  colname <- col_vars[1]
  designs <- scf$mi_design
  nimp <- length(designs)
  
  first_vars <- designs[[1]]$variables
  
  if (!rowname %in% names(first_vars)) {
    stop("Row variable not found in SCF data: ", rowname, call. = FALSE)
  }
  
  if (!colname %in% names(first_vars)) {
    stop("Column variable not found in SCF data: ", colname, call. = FALSE)
  }
  
  row_label <- if (!is.null(attr(first_vars[[rowname]], "label"))) {
    attr(first_vars[[rowname]], "label")
  } else {
    rowname
  }
  
  col_label <- if (!is.null(attr(first_vars[[colname]], "label"))) {
    attr(first_vars[[colname]], "label")
  } else {
    colname
  }
  
  imp_tables <- vector("list", nimp)
  
  row_levels <- unique(unlist(lapply(designs, function(d) {
    levels(factor(d$variables[[rowname]]))
  })))
  
  col_levels <- unique(unlist(lapply(designs, function(d) {
    levels(factor(d$variables[[colname]]))
  })))
  
  for (i in seq_len(nimp)) {
    d <- designs[[i]]
    d$variables[[rowname]] <- factor(d$variables[[rowname]], levels = row_levels)
    d$variables[[colname]] <- factor(d$variables[[colname]], levels = col_levels)
    
    tbl <- try(
      survey::svytable(
        as.formula(paste("~", rowname, "+", colname)),
        d
      ),
      silent = TRUE
    )
    
    if (inherits(tbl, "try-error")) next
    
    full_tbl <- matrix(
      0,
      nrow = length(row_levels),
      ncol = length(col_levels),
      dimnames = list(row_levels, col_levels)
    )
    
    full_tbl[rownames(tbl), colnames(tbl)] <- tbl
    imp_tables[[i]] <- full_tbl
  }
  
  imp_tables <- Filter(Negate(is.null), imp_tables)
  
  if (length(imp_tables) < 2L) {
    stop("Too few valid implicates.", call. = FALSE)
  }
  
  pooled_table <- Reduce("+", imp_tables) / length(imp_tables)
  total_pop <- sum(pooled_table)
  
  grid <- expand.grid(
    row = row_levels,
    col = col_levels,
    stringsAsFactors = FALSE
  )
  grid$rowvar <- rowname
  grid$colvar <- colname
  
  imp_counts <- lapply(seq_along(imp_tables), function(i) {
    tab <- imp_tables[[i]]
    data.frame(
      row = rep(rownames(tab), times = ncol(tab)),
      col = rep(colnames(tab), each = nrow(tab)),
      count = as.vector(tab),
      implicate = i,
      stringsAsFactors = FALSE
    )
  })
  
  long <- do.call(rbind, imp_counts)
  
  pooled_stats <- do.call(rbind, lapply(
    split(long, list(long$row, long$col), drop = TRUE),
    function(df) {
      x <- df$count
      m <- length(x)
      qbar <- mean(x)
      b <- var(x)
      se <- sqrt((1 + 1 / m) * b)
      
      data.frame(
        row = df$row[1],
        col = df$col[1],
        prop = qbar / total_pop,
        se = se / total_pop,
        stringsAsFactors = FALSE
      )
    }
  ))
  
  stat_df <- merge(
    grid,
    pooled_stats,
    by = c("row", "col"),
    all.x = TRUE,
    sort = FALSE
  )
  
  stat_df$prop[is.na(stat_df$prop)] <- 0
  stat_df$se[is.na(stat_df$se)] <- 0
  
  stat_df$row_share <- ave(stat_df$prop, stat_df$row, FUN = function(x) {
    if (sum(x) == 0) rep(NA_real_, length(x)) else x / sum(x)
  })
  
  stat_df$col_share <- ave(stat_df$prop, stat_df$col, FUN = function(x) {
    if (sum(x) == 0) rep(NA_real_, length(x)) else x / sum(x)
  })
  
  to_matrix <- function(df, value_col) {
    xtabs(df[[value_col]] ~ df$row + df$col)
  }
  
  matrices <- list(
    cell = to_matrix(stat_df, "prop"),
    row  = to_matrix(stat_df, "row_share"),
    col  = to_matrix(stat_df, "col_share"),
    se   = to_matrix(stat_df, "se")
  )
  
  out <- list(
    results = stat_df,
    matrices = matrices,
    imps = imp_tables,
    aux = list(
      rowvar = rowname,
      colvar = colname,
      rowlabel = row_label,
      collabel = col_label,
      scale = scale
    )
  )
  
  class(out) <- "scf_xtab"
  out
}

#' @export
print.scf_xtab <- function(x, ...) {
  cat("SCF Cross-Tabulation\n")
  cat("Row Variable:", x$aux$rowlabel, "| Column Variable:", x$aux$collabel, "\n")
  cat("Displayed as:", x$aux$scale, "proportions (percent)\n\n")

  mat <- round(100 * x$matrices[[x$aux$scale]], 2)
  dimnames(mat) <- list(
    paste0(x$aux$rowlabel, ": ", rownames(mat)),
    paste0(x$aux$collabel, ": ", colnames(mat))
  )
  print(mat)
  invisible(x)
}

#' @export
summary.scf_xtab <- function(object, ...) {
  cat("Summary of SCF Cross-Tabulation\n")
  cat("Row Variable:", object$aux$rowlabel, "\n")
  cat("Col Variable:", object$aux$collabel, "\n")
  cat("Proportion Scale:", object$aux$scale, "\n\n")

  cat("Proportions (Percent):\n")
  print(round(100 * object$matrices[[object$aux$scale]], 2))

  cat("\nStandard Errors (Proportions):\n")
  print(round(object$matrices$se, 4))
  invisible(object)
}
