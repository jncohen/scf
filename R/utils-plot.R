.scf_relabel <- function(x, map) {
  if (is.null(map)) return(x)
  lv <- levels(factor(x))
  if (all(lv %in% names(map))) lv <- names(map)[names(map) %in% lv]
  new <- ifelse(lv %in% names(map), unname(map[lv]), lv)
  factor(as.character(x), levels = lv, labels = new)
}

.scf_comma <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}
