#' Extract Raw Implicates into Named Data Frames
#'
#' @description
#' Returns implicate data frames from an `scf_mi_survey` as a named list.
#' If `object$implicates` is `NULL`, it derives implicates from `mi_design`
#' (pulls `$variables` from each design). Optionally assigns to `.GlobalEnv`.
#'
#' @param object An `scf_mi_survey` with either `implicates` (list) or
#'   `mi_design` (list of survey designs).
#' @param label Character prefix for names. Default `"scf"`.
#' @param to_global Logical. If `TRUE`, assign each implicate into
#'   `.GlobalEnv`. Default `FALSE`.
#' @return Named list of implicate data frames (length ≥ 1).
#' @examples
#' scf2022 <- readRDS(system.file("extdata","mock_scf2022.rds",package="scf"))
#' imps <- scf_extract_implicates(scf2022, label = "mydata")
#' head(imps[[1]]$income)
#' \dontrun{
#' scf_extract_implicates(scf2022, label="mydata", to_global=TRUE)
#' head(mydata_1$income)
#' }
#' @export
scf_extract_implicates <- function(object, label = "scf", to_global = FALSE) {
  if (!is.list(object))
    stop("`object` must be a list-like scf_mi_survey.")
  
  # Preferred source: object$implicates
  imps <- object$implicates
  # Fallback: derive from mi_design if implicates missing/NULL
  if (is.null(imps) && !is.null(object$mi_design)) {
    md <- object$mi_design
    if (is.list(md)) {
      imps <- lapply(md, function(d) {
        v <- tryCatch(d$variables, error = function(e) NULL)
        if (is.null(v))
          stop("mi_design elements lack `$variables`; cannot derive implicates.")
        v
      })
    } else {
      v <- tryCatch(md$variables, error = function(e) NULL)
      if (is.null(v))
        stop("mi_design lacks `$variables`; cannot derive implicates.")
      imps <- list(v)
    }
  }
  
  if (is.null(imps))
    stop("No implicates found: provide `object$implicates` or `object$mi_design`.")
  
  if (!is.list(imps) || length(imps) < 1)
    stop("`implicates` must be a non-empty list of data frames.")
  
  if (!all(vapply(imps, is.data.frame, logical(1))))
    stop("All implicates must be data frames.")
  
  names(imps) <- paste0(label, "_", seq_along(imps))
  
  if (isTRUE(to_global)) list2env(imps, envir = .GlobalEnv)
  
  imps
}
