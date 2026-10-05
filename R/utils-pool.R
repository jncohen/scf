#' @noRd
.scf_pool <- function(est, var, method = c("fed", "rubin")) {
  method <- match.arg(method)
  est <- as.matrix(est)
  var <- as.matrix(var)
  m <- colSums(!is.na(est))
  qbar <- colMeans(est, na.rm = TRUE)
  b <- apply(est, 2, function(x) {
    if (sum(!is.na(x)) > 1) stats::var(x, na.rm = TRUE) else 0
  })
  w <- if (method == "rubin") {
    colMeans(var, na.rm = TRUE)
  } else {
    apply(var, 2, function(v) v[!is.na(v)][1])
  }
  total <- w + (1 + 1 / m) * b
  df <- ifelse(b > 0, (m - 1) * (1 + w / ((1 + 1 / m) * b))^2, Inf)
  qbar[m == 0] <- NA_real_
  total[m == 0] <- NA_real_
  list(
    estimate = unname(qbar),
    se = unname(sqrt(total)),
    df = unname(df),
    within = unname(w),
    between = unname(b)
  )
}

.scf_variance_method <- function(variance) {
  if (is.null(variance)) variance <- getOption("scf.variance", "fed")
  match.arg(variance, c("fed", "rubin"))
}

.scf_stars <- function(p) {
  cut(p,
      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.10, Inf),
      labels = c("***", "**", "*", "^", ""),
      right = FALSE)
}
