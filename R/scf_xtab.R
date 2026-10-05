#' Cross-Tabulate Two Discrete Variables in Multiply-Imputed SCF Data
#'
#' Estimates a two-way table of household shares, with standard errors.
#'
#' @details
#' Use it to see how two categorical variables relate, such as homeownership
#' by education. `scale` sets which shares print: of all households
#' (`"cell"`), of each row (`"row"`, rows sum to 100 percent and show how each
#' row divides across columns), or of each column (`"col"`).
#'
#' Shares are estimated in each implicate with the replicate weights and
#' pooled (see [scf_variance]). Counts come from `survey::svytable()`.
#'
#' @param scf A `scf_mi_survey` object, typically created by [scf_load()]. Must include five implicates with replicate weights.
#' @param rowvar A one-sided formula specifying the row variable (e.g., `~edcl`).
#' @param colvar A one-sided formula specifying the column variable (e.g., `~racecl`).
#' @param scale Character. Proportion basis: "cell" (default), "row", or "col".
#' @param variance Variance method: `"fed"` (default) or `"rubin"`. See [scf_variance].
#'
#' @return A list of class `"scf_xtab"` with:
#' \describe{
#'   \item{results}{One row per cell: `row`, `col`, `rowvar`, `colvar`, `prop` and `se` (share of all households), `row_share`, `col_share`, `row_share_se`, and `col_share_se`.}
#'   \item{matrices}{Matrices of `cell`, `row`, and `col` shares, and their standard errors `se`, `se_row`, and `se_col`.}
#'   \item{imps}{Weighted cell counts for each implicate.}
#'   \item{aux}{List with `rowvar` and `colvar` names.}
#' }
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
#'   scf_xtab(scf2022, ~own, ~hhsex, scale = "row")
#' }
#'
#' # Do not implement these lines in real analysis: Cleanup for package check
#' unlink(td, recursive = TRUE, force = TRUE)
#'
#' @importFrom stats as.formula ave
#' @export
scf_xtab <- function(scf, rowvar, colvar, scale = "cell",
                     variance = getOption("scf.variance", "fed")) {
  variance <- .scf_variance_method(variance)
  scale <- match.arg(scale, choices = c("cell", "row", "col"))

  if (!inherits(scf, "scf_mi_survey")) {
    stop("Input must be an 'scf_mi_survey' object.", call. = FALSE)
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

  .scf_check_na(designs, rowvar, colvar)

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

  row_levels <- .scf_group_levels(designs, rowname)
  col_levels <- .scf_group_levels(designs, colname)

  for (i in seq_len(nimp)) {
    d <- designs[[i]]
    d$variables[[rowname]] <- factor(d$variables[[rowname]], levels = row_levels)
    d$variables[[colname]] <- factor(d$variables[[colname]], levels = col_levels)

    tbl <- survey::svytable(as.formula(paste("~", rowname, "+", colname)), d)

    full_tbl <- matrix(
      0,
      nrow = length(row_levels),
      ncol = length(col_levels),
      dimnames = list(row_levels, col_levels)
    )

    full_tbl[rownames(tbl), colnames(tbl)] <- tbl
    imp_tables[[i]] <- full_tbl
  }

  n_cells <- length(row_levels) * length(col_levels)
  cell_key <- paste(rep(row_levels, times = length(col_levels)),
                    rep(col_levels, each = length(row_levels)), sep = "\r")

  grab <- function(est, se, keys) {
    out_est <- stats::setNames(rep(NA_real_, n_cells), cell_key)
    out_var <- out_est
    hit <- keys %in% cell_key
    out_est[keys[hit]] <- est[hit]
    out_var[keys[hit]] <- se[hit]^2
    list(est = out_est, var = out_var)
  }

  imp_shares <- lapply(seq_len(nimp), function(i) {
    d <- designs[[i]]
    d$variables[[rowname]] <- factor(d$variables[[rowname]], levels = row_levels)
    d$variables[[colname]] <- factor(d$variables[[colname]], levels = col_levels)
    d$variables$.cell <- factor(
      paste(d$variables[[rowname]], d$variables[[colname]], sep = "\r"),
      levels = cell_key
    )
    d$variables$.cell[is.na(d$variables[[rowname]]) | is.na(d$variables[[colname]])] <- NA

    cell <- survey::svymean(~.cell, d, na.rm = TRUE)
    cell_keys <- sub("^\\.cell", "", names(coef(cell)))

    by_row <- survey::svyby(stats::as.formula(paste0("~", colname)),
                            stats::as.formula(paste0("~", rowname)),
                            d, survey::svymean, na.rm = TRUE, covmat = TRUE)
    by_col <- survey::svyby(stats::as.formula(paste0("~", rowname)),
                            stats::as.formula(paste0("~", colname)),
                            d, survey::svymean, na.rm = TRUE, covmat = TRUE)

    gr <- as.character(by_row[[rowname]])
    gc <- as.character(by_col[[colname]])
    stopifnot(length(coef(by_row)) == length(gr) * length(col_levels),
              length(coef(by_col)) == length(gc) * length(row_levels))
    row_keys <- paste(rep(gr, times = length(col_levels)),
                      rep(col_levels, each = length(gr)), sep = "\r")
    col_keys <- paste(rep(row_levels, each = length(gc)),
                      rep(gc, times = length(row_levels)), sep = "\r")

    se_of <- function(x) sqrt(diag(as.matrix(stats::vcov(x))))
    list(
      cell = grab(coef(cell), se_of(cell), cell_keys),
      row = grab(coef(by_row), se_of(by_row), row_keys),
      col = grab(coef(by_col), se_of(by_col), col_keys)
    )
  })

  pool <- function(part) {
    est <- do.call(rbind, lapply(imp_shares, function(x) x[[part]]$est))
    var <- do.call(rbind, lapply(imp_shares, function(x) x[[part]]$var))
    p <- .scf_pool(est, var, variance)
    list(est = p$estimate, se = p$se)
  }

  cell_p <- pool("cell")
  row_p <- pool("row")
  col_p <- pool("col")

  stat_df <- expand.grid(
    row = row_levels,
    col = col_levels,
    stringsAsFactors = FALSE
  )
  stat_df$rowvar <- rowname
  stat_df$colvar <- colname
  stat_df$prop <- ifelse(is.na(cell_p$est), 0, cell_p$est)
  stat_df$se <- ifelse(is.na(cell_p$se), 0, cell_p$se)
  stat_df$row_share <- row_p$est
  stat_df$col_share <- col_p$est
  stat_df$row_share_se <- row_p$se
  stat_df$col_share_se <- col_p$se

  to_matrix <- function(v) {
    matrix(v, nrow = length(row_levels),
           dimnames = stats::setNames(list(row_levels, col_levels), c(rowname, colname)))
  }

  matrices <- list(
    cell = to_matrix(stat_df$prop),
    row = to_matrix(stat_df$row_share),
    col = to_matrix(stat_df$col_share),
    se = to_matrix(stat_df$se),
    se_row = to_matrix(stat_df$row_share_se),
    se_col = to_matrix(stat_df$col_share_se)
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
  se_name <- switch(object$aux$scale, cell = "se", row = "se_row", col = "se_col")
  print(round(object$matrices[[se_name]], 4))
  invisible(object)
}
