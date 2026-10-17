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

.scf_warnings <- function() {
  env <- new.env(parent = emptyenv())
  env$msg <- character(0)
  env$imp <- integer(0)
  env
}

.scf_catch_warnings <- function(expr, i, env) {
  withCallingHandlers(expr, warning = function(w) {
    env$msg <- c(env$msg, conditionMessage(w))
    env$imp <- c(env$imp, i)
    invokeRestart("muffleWarning")
  })
}

.scf_report_warnings <- function(env) {
  for (msg in unique(env$msg)) {
    imps <- sort(unique(env$imp[env$msg == msg]))
    warning(sprintf("%s (implicate%s %s)", msg, if (length(imps) > 1) "s" else "",
                    paste(imps, collapse = ", ")), call. = FALSE)
  }
}

.scf_group_n <- function(designs, byname = NULL, groups = NULL) {
  if (is.null(byname)) {
    return(round(mean(sapply(designs, function(d) nrow(d$variables)))))
  }
  counts <- sapply(designs, function(d) {
    g <- d$variables[[byname]]
    sapply(groups, function(k) sum(!is.na(g) & as.character(g) == k))
  })
  round(rowMeans(matrix(counts, nrow = length(groups))))
}

.scf_warn_small <- function(n, groups = NULL, limit = 10) {
  small <- which(n <= limit)
  if (length(small) == 0) return(invisible(NULL))
  what <- if (is.null(groups)) {
    "The estimate rests on"
  } else if (length(small) == 1) {
    sprintf("Group `%s` has", groups[small])
  } else {
    sprintf("Groups %s have", paste(sprintf("`%s`", groups[small]), collapse = ", "))
  }
  warning(sprintf(paste0("%s %s households. Estimates from %d or fewer households ",
                         "are not reliable; see the `n` column."),
                  what, paste(n[small], collapse = ", "), limit), call. = FALSE)
}

.scf_shown_results <- function(r) {
  if ("se" %in% names(r) && all(is.na(r$se))) {
    r <- r[setdiff(names(r), c("se", "min", "max"))]
  }
  r
}
