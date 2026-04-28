#' Internal Import Declarations
#'
#' Declares functions from base packages used in nonstandard evaluation or
#' dynamic contexts across `scf` package functions. Ensures all used base
#' functions are properly registered in the NAMESPACE.
#'
#' @keywords internal
#' @importFrom stats var pnorm qnorm qt setNames binomial family xtabs aggregate IQR weighted.mean
#' @importFrom utils head tail unzip write.csv
#' @importFrom rlang .data
#' @importFrom survey SE cv
#' @import ggplot2
#' @name scf_imports
NULL

# .wts is the replicate-weight vector injected by survey::withReplicates()
# into user-supplied functions. Declared here to suppress R CMD check NOTE.
utils::globalVariables(".wts")
