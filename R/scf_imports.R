#' Internal Import Declarations
#'
#' Declares functions from base packages used in nonstandard evaluation or
#' dynamic contexts across `scf` package functions. Ensures all used base
#' functions are properly registered in the NAMESPACE.
#'
#' @keywords internal
#' @importFrom stats var pnorm qnorm qt setNames binomial family xtabs aggregate IQR weighted.mean
#' @importFrom utils head tail unzip install.packages write.csv
#' @importFrom rlang .data :=
#' @import ggplot2
#' @name scf_imports
NULL
