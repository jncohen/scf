# Internal helper: weighted quantile.
#
# Finds the first observation at which the cumulative weighted population
# share reaches or exceeds each requested probability.
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
  
  as.numeric(vapply(probs, function(p) {
    idx <- which(cum_w >= p)[1L]
    if (is.na(idx)) NA_real_ else x[idx]
  }, numeric(1)))
}


# Internal helper: weighted median.
#
# Used by scf_pctile_sum(method = "stack", stat = "median").
.scf_wtd_median <- function(x, w) {
  .scf_wtd_quantile(x = x, w = w, probs = 0.5)
}