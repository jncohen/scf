#' Internal Import Declarations
#'
#' Declares functions from other packages used across `scf` functions, so
#' they are registered in the NAMESPACE.
#'
#' @keywords internal
#' @importFrom stats var pnorm qnorm qt setNames binomial family xtabs aggregate IQR weighted.mean
#' @importFrom utils head tail unzip write.csv
#' @importFrom survey SE cv
#' @import ggplot2
#' @name scf_imports
NULL

utils::globalVariables(".wts")
