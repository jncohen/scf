.scf_wtd_quantile <- function(x, w, probs) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]
  w <- w[ok]

  if (length(x) == 0L) {
    return(rep(NA_real_, length(probs)))
  }

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cum_w <- cumsum(w) / sum(w)

  out <- numeric(length(probs))
  for (k in seq_along(probs)) {
    idx <- which(cum_w >= probs[k])[1L]
    out[k] <- if (is.na(idx)) NA_real_ else x[idx]
  }
  out
}

.scf_wtd_median <- function(x, w) {
  .scf_wtd_quantile(x = x, w = w, probs = 0.5)
}
