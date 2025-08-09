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
#' \donttest{
#' # Load bundled mock data (for demonstration only — not real SCF data)
#' scf2022 <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))

#' # For real analysis, use:
#' # scf_download(2022); scf2022 <- scf_load(2022)
#'
#' scf_xtab(scf2022, ~own, ~edcl, scale = "row")
#' }
#'
#' @importFrom stats as.formula ave
#' @export
scf_xtab <- function(scf, rowvar, colvar, scale = "cell") {
  scale <- match.arg(scale, choices = c("cell", "row", "col"))

  if (isTRUE(attr(scf, "mock"))) {
    warning("Mock data detected. Do not interpret results as valid SCF estimates.", call. = FALSE)
  }


  if (!inherits(scf, "scf_mi_survey"))
    stop("Input must be an 'scf_mi_survey' object.")

  rowname <- all.vars(rowvar)[1]
  colname <- all.vars(colvar)[1]
  designs <- scf$mi_design
  nimp <- length(designs)

  imp_tables <- vector("list", nimp)
  row_levels <- NULL
  col_levels <- NULL
  row_label <- if (!is.null(attr(scf$data[[rowname]], "label"))) attr(scf$data[[rowname]], "label") else rowname
  col_label <- if (!is.null(attr(scf$data[[colname]], "label"))) attr(scf$data[[colname]], "label") else colname

  for (i in seq_len(nimp)) {
    d <- designs[[i]]
    d$variables[[rowname]] <- factor(d$variables[[rowname]])
    d$variables[[colname]] <- factor(d$variables[[colname]])

    if (is.null(row_levels)) row_levels <- levels(d$variables[[rowname]])
    if (is.null(col_levels)) col_levels <- levels(d$variables[[colname]])

    tbl <- try(survey::svytable(as.formula(paste("~", rowname, "+", colname)), d), silent = TRUE)
    if (inherits(tbl, "try-error")) next

    full_tbl <- matrix(0, nrow = length(row_levels), ncol = length(col_levels),
                       dimnames = list(row_levels, col_levels))
    full_tbl[rownames(tbl), colnames(tbl)] <- tbl
    imp_tables[[i]] <- full_tbl
  }

  imp_tables <- Filter(Negate(is.null), imp_tables)
  if (length(imp_tables) < 2L) stop("Too few valid implicates.")

  pooled_table <- Reduce("+", imp_tables) / length(imp_tables)
  total_pop <- sum(pooled_table)

  grid <- expand.grid(row = row_levels, col = col_levels, stringsAsFactors = FALSE)
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

  pooled_stats <- lapply(split(long, list(long$row, long$col)), function(df) {
    x <- df$count
    m <- length(x)
    qbar <- mean(x)
    b <- var(x)
    se <- sqrt((1 + 1 / m) * b)
    data.frame(prop = qbar / total_pop, se = se / total_pop, stringsAsFactors = FALSE)
  })

  stat_df <- cbind(grid[, c("row", "col", "rowvar", "colvar")], do.call(rbind, pooled_stats))

  stat_df$row_share <- ave(stat_df$prop, stat_df$row, FUN = function(x) x / sum(x))
  stat_df$col_share <- ave(stat_df$prop, stat_df$col, FUN = function(x) x / sum(x))

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
    aux = list(rowvar = rowname, colvar = colname, rowlabel = row_label, collabel = col_label, scale = scale)
  )
  class(out) <- "scf_xtab"
  return(out)
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
