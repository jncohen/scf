.scf_state <- new.env(parent = emptyenv())

.scf_svrepdesign <- function(df) {
  rep_cols <- grep("^wt1b", names(df), value = TRUE)
  if (length(rep_cols) == 0) {
    stop("Could not find replicate weight columns (wt1b*).", call. = FALSE)
  }
  survey::svrepdesign(
    weights = df$wgt * 5,
    repweights = as.matrix(df[, rep_cols]),
    data = df,
    type = "other",
    scale = 1,
    rscales = rep(1 / (length(rep_cols) - 1), length(rep_cols)),
    mse = FALSE,
    combined.weights = TRUE
  )
}

.scf_group_levels <- function(designs, name) {
  vals <- lapply(designs, function(d) d$variables[[name]])
  seen <- unique(unlist(lapply(vals, function(v) as.character(v[!is.na(v)]))))
  first <- vals[[1]]
  lv <- if (is.factor(first)) {
    c(levels(first), sort(setdiff(seen, levels(first))))
  } else if (is.numeric(first)) {
    as.character(sort(unique(unlist(lapply(vals, function(v) v[!is.na(v)])))))
  } else {
    sort(seen)
  }
  lv[lv %in% seen]
}

.scf_check_na <- function(designs, ...) {
  forms <- Filter(Negate(is.null), list(...))
  for (i in seq_along(designs)) {
    df <- designs[[i]]$variables
    for (f in forms) {
      mf <- stats::model.frame(f, df, na.action = stats::na.pass)
      for (nm in names(mf)) {
        n <- sum(is.na(mf[[nm]]))
        if (n > 0) {
          stop(sprintf(paste0(
            "`%s` has %d missing values in implicate %d. SCF data have no ",
            "missing values, so check how it was made. Fix the variable ",
            "(for example, bottom-code it before taking logs) or use ",
            "scf_subset() to leave these households out."), nm, n, i), call. = FALSE)
        }
      }
    }
  }
  invisible(TRUE)
}

.scf_eval <- function(f, df) {
  eval(f[[2]], df, environment(f))
}

.scf_lighten_fit <- function(m, env) {
  m$survey.design <- NULL
  m$data <- NULL
  if (!is.null(m$formula)) environment(m$formula) <- env
  if (!is.null(m$terms)) environment(m$terms) <- env
  if (!is.null(m$model)) attr(m$model, "terms") <- m$terms
  m
}

.scf_fit_error <- function(i, e) {
  msg <- sub("[.[:space:]]+$", "", conditionMessage(e))
  hint <- if (grepl("NA values in estimate|singular", msg)) {
    " Check for terms that are perfectly collinear."
  } else {
    ""
  }
  stop(sprintf("The model failed in implicate %d: %s.%s", i, msg, hint),
       call. = FALSE)
}

.scf_align_terms <- function(coefs_list, vars_list) {
  terms <- unique(unlist(lapply(coefs_list, names)))
  for (i in seq_along(coefs_list)) {
    miss <- setdiff(terms, names(coefs_list[[i]]))
    if (length(miss) > 0) {
      stop(sprintf(paste0(
        "%s is missing from implicate %d. This happens when a factor level ",
        "is empty in some implicates. Combine or drop the level."),
        paste0("`", miss, "`", collapse = ", "), i), call. = FALSE)
    }
  }
  nm <- names(coefs_list[[1]])
  list(
    coefs = lapply(coefs_list, function(x) x[nm]),
    vars = lapply(vars_list, function(v) v[nm, nm, drop = FALSE])
  )
}
