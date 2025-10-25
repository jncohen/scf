#' Extract Standard Errors from MIresult Object
#'
#' Generic method to extract standard errors from multiply-imputed
#' Rubin-pooled model output of class `"scf_MIresult"`.
#'
#' @param object An object of class `"scf_MIresult"` created by [scf_MIcombine()].
#' @param ... Passed to method (not used).
#'
#' @return A numeric vector of standard errors corresponding to each coefficient.
#'
#' @keywords internal
#' @noRd
SE <- function(object, ...) UseMethod("SE")

SE.MIresult <- function(object, ...) {
  sqrt(diag(object$variance))
}
