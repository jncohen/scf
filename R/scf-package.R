#' Analyzing Survey of Consumer Finances Public-Use Microdata
#'
#' @author Joseph N. Cohen, CUNY Queens College
#'
#' @description
#' Functions to analyze the Federal Reserve's Survey of Consumer Finances
#' (SCF) public-use microdata. The package stores the SCF's five implicates and
#' replicate weights in one object (`scf_mi_survey`) and provides means,
#' medians, percentiles, frequencies, cross-tabs, ratios, tests, regression
#' models, and plots.
#'
#' It is written for analysts who know standard statistics but not the
#' details of multiply imputed or survey-weighted data.
#'
#' @section Methodological Background:
#'
#' The SCF is a detailed source of data on U.S. household finances. It is
#' nationally representative, oversamples wealthy households, and fills in
#' missing answers by multiple imputation. Valid estimates need three things:
#'
#' - Sampling weights. The SCF uses a dual-frame probability sample, so
#'   estimates must use the weights.
#' - Replicate weights. Each household has 999 bootstrap replicate weights,
#'   built by the Fed, for estimating sampling variance.
#' - Multiple imputation. Each household appears in five implicates.
#'   Estimates are pooled across them to get point estimates and standard
#'   errors.
#'
#' The package handles all three. See [scf_variance] for how standard errors
#' are computed.
#'
#' @section Workflow:
#'
#' For the methods behind these functions, see Cohen (2026).
#'
#' 1. Download the data with [scf_download()].
#' 2. Load it with [scf_load()], which returns a `scf_mi_survey` object.
#' 3. Change variables with [scf_update()] or [scf_update_by_implicate()], and
#'    keep a subset of households with [scf_subset()].
#' 4. Describe the data with [scf_mean()], [scf_median()], [scf_percentile()],
#'    [scf_freq()], [scf_xtab()], [scf_ratio()], [scf_corr()], and
#'    [scf_pctile_sum()].
#' 5. Test hypotheses with [scf_ttest()] and [scf_prop_test()].
#' 6. Fit models with [scf_ols()], [scf_logit()], [scf_glm()], and
#'    [scf_quantreg()], and tabulate them with [scf_regtable()].
#' 7. Plot with [scf_plot_dist()], [scf_plot_hist()], [scf_plot_dbar()],
#'    [scf_plot_cbar()], [scf_plot_bbar()], [scf_plot_smooth()], and
#'    [scf_plot_hex()].
#' 8. Convert raw `x` dollar variables to 2022 dollars with [scf_real()],
#'    and dollar results to nominal survey-year dollars with [scf_nominal()].
#'
#' @section Households:
#' The SCF's unit is the primary economic unit: the person or couple at the
#' economic center of the household and everyone financially dependent on
#' them. The Fed calls this unit a "family"; this package calls it a
#' household.
#'
#' @section Dollars:
#' Summary variables made by the Fed, such as `income`, `networth`, and
#' `asset`, are in 2022 dollars in every survey year, so they can be compared
#' across years as they are. Raw survey variables, named `x` followed by a
#' number, are in dollars of the survey year. Convert them with [scf_real()]
#' before comparing years or combining them with summary variables.
#'
#' @section Data Object:
#'
#' Functions work on a `scf_mi_survey` object, made by [scf_design()] via
#' [scf_load()]. It is a list with:
#'
#' - `mi_design`: five [survey::svrepdesign()] objects, one per implicate
#' - `year`: the survey year
#' - `n_households`: the weighted number of households
#'
#' @section Imputed Missing Data:
#'
#' The SCF fills in missing answers by multiple imputation (Kennickell 1998).
#' A model of the observed data is used to draw five plausible values for each
#' missing answer, giving five complete data sets, called implicates. The
#' spread across implicates shows how uncertain the filled-in values are. See
#' [scf_variance] and [scf_MIcombine()].
#'
#' @section Mock Data for Testing:
#'
#' A mock SCF data set (`scf2022_mock_raw.rds`) in `inst/extdata/` is used in
#' examples and tests. It keeps the first 200 rows of each implicate and only
#' the variables the examples use. It is not suitable for analysis or
#' inference.
#'
#' @section Plot Theme:
#' All plots use [scf_theme()]. See its help page to change the theme.
#'
#' @section Teaching:
#'
#' The package can be used to teach survey analysis and missing data, because
#' each step can be inspected:
#'
#' - Each implicate's design is in `x$mi_design[[i]]`.
#' - Each implicate's data is in `x$mi_design[[i]]$variables`.
#' - Results keep implicate-level estimates (see [scf_implicates()]).
#' - Standard errors come from replicate weights and the spread across
#'   implicates, so students can follow how each part adds to the total.
#'
#' @references
#' Cohen JN. Analyzing the Survey of Consumer Finances with \pkg{scf}:
#'   Methodology, User Guide, and Package Validation. Working paper, CUNY
#'   Queens College; 2026. <https://academicworks.cuny.edu/qc_pubs/695/>
#'
#' Barnard J, Rubin DB. Small-sample degrees of freedom with multiple
#'   imputation. \emph{Biometrika}. 1999;86(4):948--955.
#'   \doi{10.1093/biomet/86.4.948}
#'
#' Board of Governors of the Federal Reserve System. Codebook for the 2022
#'   Survey of Consumer Finances.
#'   <https://www.federalreserve.gov/econres/scfindex.htm>
#'
#' Bricker J, Henriques AM, Moore KB. Updates to the sampling of wealthy
#'   families in the Survey of Consumer Finances. Finance and Economics
#'   Discussion Series 2017-114. Board of Governors of the Federal Reserve
#'   System; 2017.
#'
#' Kennickell AB. Multiple imputation in the Survey of Consumer Finances.
#'   Board of Governors of the Federal Reserve System; 1998.
#'
#' Kennickell AB. Multiple imputation in the Survey of Consumer Finances.
#'   \emph{Statistical Journal of the IAOS}. 2017;33(1):143--151.
#'   \doi{10.3233/SJI-160278}
#'
#' Kennickell AB, McManus DA, Woodburn RL. Weighting design for the 1992
#'   Survey of Consumer Finances. Board of Governors of the Federal Reserve
#'   System; 1996.
#'   <https://www.federalreserve.gov/Pubs/OSS/oss2/papers/weight92.pdf>
#'
#' Little RJA, Rubin DB. \emph{Statistical Analysis with Missing Data}.
#'   3rd ed. Wiley; 2019. ISBN: 9780470526798.
#'
#' Lumley T. Analysis of complex survey samples. \emph{Journal of
#'   Statistical Software}. 2004;9(8):1--19. \doi{10.18637/jss.v009.i08}
#'
#' Lumley T. \emph{Complex Surveys: A Guide to Analysis Using R}. Wiley;
#'   2010. ISBN: 9781118210932.
#'
#' Lumley T. survey: Analysis of complex survey samples. R package.
#'   <https://CRAN.R-project.org/package=survey>
#'
#' Rubin DB. \emph{Multiple Imputation for Nonresponse in Surveys}. Wiley;
#'   1987.
#'
#' @name scf
"_PACKAGE"
