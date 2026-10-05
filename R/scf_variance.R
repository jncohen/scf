#' Standard Errors Across SCF Implicates
#'
#' @description
#' Every SCF estimate has two sources of error. `scf` adds them to get the
#' standard error, using Rubin's rules (see [scf_MIcombine]). This page
#' explains the first source, sampling error, and the two ways `scf` can
#' measure it.
#'
#' Sampling error. The SCF interviews a sample of households, not all of
#' them. A different sample would give a slightly different answer. The Fed
#' measures this with 999 replicate weights: each one re-weights the sample as
#' if it had been drawn again. `scf` re-computes the estimate with each set of
#' replicate weights. The sampling variance \eqn{U} is the spread of those 999
#' answers: the sum of their squared distances from their mean, divided by
#' 998.
#'
#' Imputation error. Some answers are missing, and the Fed fills them in
#' five times, giving five implicates. The spread of the five implicate
#' estimates is the imputation variance \eqn{B}. See [scf_MIcombine].
#'
#' @section Two ways to measure sampling variance:
#' Each implicate has its own sampling variance. The two methods differ only
#' in which ones are used for \eqn{U}.
#'
#' \describe{
#'   \item{`"fed"` (default)}{\eqn{U} is the sampling variance from the first
#'     implicate. This is what the Fed's own SAS code (the MEANIT macro in the
#'     SCF codebook) does for means and medians. The Fed built the replicate
#'     weights for the first implicate, so this method uses them on the data
#'     they were made for.}
#'   \item{`"rubin"`}{\eqn{U} is the average sampling variance over all five
#'     implicates. This is standard multiple-imputation practice (Rubin 1987)
#'     and what most software does, including `mitools` and Stata's `mi`. It
#'     uses more information, so it is slightly more stable.}
#' }
#'
#' For estimates on the whole population, the two methods give nearly the same
#' answer. They can differ for groups whose membership depends on imputed
#' values, such as households grouped by net worth or income. A household can
#' fall in the group in one implicate and out of it in another, so the first
#' implicate's group may not represent the others. The Fed's documentation
#' warns about this case.
#'
#' Regression models ([scf_ols()], [scf_glm()], [scf_logit()],
#' [scf_quantreg()]) always use `"rubin"`, as the Fed's codebook does for
#' regressions (the MISECOMP macro).
#'
#' If a group is missing from some implicates, `"fed"` uses the first
#' implicate that has it.
#'
#' @section Subsets and groups:
#' Take care when [scf_subset()] or `by = ` uses a variable that the Fed
#' imputes, such as income, net worth, assets, or debts. Each implicate can
#' then hold a different set of households. With `"fed"`, the sampling
#' variance comes from the first implicate's set alone, which may not
#' represent the other four. With `"rubin"`, it is averaged over all five
#' sets, so every version of the group counts.
#'
#' For example, `scf_subset(scf2022, networth > 1e6)` keeps a different
#' set of households in each implicate. The same is true of groups made from
#' imputed variables, such as net worth categories. Use `"rubin"` for analyses
#' like these:
#'
#' \preformatted{
#' rich <- scf_subset(scf2022, networth > 1e6)
#' scf_mean(rich, ~income, variance = "rubin")
#'
#' scf_mean(scf2022, ~income, by = ~nwcat, variance = "rubin")
#' scf_freq(scf2022, ~own, by = ~nwcat, variance = "rubin")
#' }
#'
#' If the subset or group uses a variable that is rarely or never imputed,
#' such as age or household sex, the households are nearly the same in every
#' implicate and the two methods agree.
#'
#' @section Which to use:
#' - Use the default, `"fed"`, to match the Fed's published methods and
#'   figures.
#' - Use `"rubin"` when groups or subsets depend on imputed values (for
#'   example `by = ~nwcat`, or `scf_subset()` on income or net worth), or when
#'   results must match other multiple-imputation software.
#' - Report which method you used. For whole-population estimates the choice
#'   rarely matters.
#'
#' @section How to set it:
#' Set it for one call with the `variance` argument:
#'
#' \preformatted{
#' scf_mean(scf2022, ~networth, variance = "rubin")
#' }
#'
#' Or set it for the whole session:
#'
#' \preformatted{
#' options(scf.variance = "rubin")
#' }
#'
#' The `variance` argument is available in [scf_mean()], [scf_median()],
#' [scf_percentile()], [scf_freq()], [scf_xtab()], [scf_corr()], [scf_ttest()],
#' [scf_prop_test()], and [scf_ratio()]. Functions that call these, such as
#' [scf_pctile_sum()] and the plot functions, follow the session setting.
#'
#' @references
#' Rubin DB. Multiple Imputation for Nonresponse in Surveys. Wiley, 1987.
#'
#' Board of Governors of the Federal Reserve System. Codebook for the Survey of
#' Consumer Finances (see the sections on sampling error and the MEANIT SAS
#' macro). \url{https://www.federalreserve.gov/econres/scfindex.htm}
#'
#' @name scf_variance
NULL
