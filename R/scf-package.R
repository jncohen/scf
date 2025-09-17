#' Analyzing Survey of Consumer Finances Public-Use Microdata
#'
#' @author Joseph N. Cohen, CUNY Queens College
#'
#' @description
#'
#' This package provides functions to analyze the U.S. Federal Reserve's
#' Survey of Consumer Finances (SCF) public-use microdata. It encapsulates
#' the SCF’s multiply-imputed, replicate-weighted structure in a custom
#' object class (`scf_mi_survey`) and supports estimation of population-level
#' statistics, including univariate and bivariate distributions,
#' hypothesis tests, data visualizations, and regression models.
#'
#' Designed for generalist analysts, the package assumes familiarity with
#' standard statistical methods but not with the complexities of
#' multiply-imputed or survey-weighted data. All functions prioritize
#' transparency, reproducibility, and pedagogical clarity.
#'
#' @section Methodological Background:
#'
#' The SCF is one of the most detailed and methodologically rigorous sources of
#' data on U.S. household finances. It is nationally representative, includes an
#' oversample of high-wealth households and households in predominantly Black
#' communities, and provides multiply-imputed estimates for item nonresponse.
#' These features increase the analytical value of the data set but also
#' introduce methodological complexity. Valid inference requires attention to:
#'
#' - **Survey Weights:** The SCF employs a dual-frame, stratified, and clustered
#'   probability sample. Analysts must apply the provided sampling weights to
#'   produce population-representative estimates.
#' - **Replicate Weights:** Each observation is associated with 999 replicate weights,
#'   generated using a custom replication method developed by the Federal
#'   Reserve. These are used to estimate sampling variance.
#' - **Multiple Imputation:** The SCF uses multiple imputation to address item nonresponse,
#'   providing five implicates per household. Estimates must be pooled across
#'   implicates to obtain valid point estimates and standard errors.
#'
#' The `scf` package provides a structured, user-friendly interface for handling
#' these design complexities, enabling applied researchers and generalist
#' analysts to conduct principled and reproducible analysis of SCF microdata
#' using familiar statistical workflows.
#'
#' @section Package Architecture and Workflow:
#'
#' This section recommends a sequence of operations enacted through the package's
#' functions.  For an in-depth discussion of the methodological considerations
#' involved in these functions' formulation, see Cohen (2025).
#'
#' 1. **Data Acquisition**:  Download the data from Federal Reserve servers to your working directory using [scf_download()].
#' 2. **Data Loading**: Load the data into R using [scf_load()]. This function returns an `scf_mi_survey` object (described below).
#' 3. **Data Wrangling**: Use [scf_update()] to modify the data, or [scf_subset()] to filter it. These functions return new `scf_mi_survey` objects.
#' 4. **Descriptive Statistics**: Compute univariate and bivariate statistics using functions like [scf_mean()], [scf_median()], [scf_percentile()], [scf_freq()], [scf_xtab()], and [scf_corr()].
#' 5. **Basic Inferential Tests**: Conduct hypothesis tests using [scf_ttest()] for means and [scf_prop_test()] for proportions.
#' 6. **Regression Modeling**: Fit regression models using [scf_ols()] for linear regression, [scf_logit()] for logistic regression, and [scf_glm()] for generalized linear models.
#' 7. **Data Visualization**: Create informative visualizations using [scf_plot_dist()] for distributions, [scf_plot_cbar()] and [scf_plot_bbar()] for categorical data, [scf_plot_smooth()] for smoothers, and [scf_plot_hex()] for hexbin plots.
#' 8. **Diagnostics and Infrastructure**: Use [scf_MIcombine()] to pool results across implicates.
#'
#' @section Core Data Object and Its Structure:
#'
#' This suite of functions operate from a custom object class, `scf_mi_survey`,
#' which is created by [scf_design()] via [scf_load()].  Specifically, the
#' object is a structured list containing the elements:
#'
#' - `mi_design`: A list of five `survey::svrepdesign()` objects (one per implicate)
#' - `year`: Year of survey
#' - `n_households`: Estimated number of U.S. households in that year, per data from the Federal Reserve Economic Data (FRED) series TTLHH, accessed 6/17/2025.
#'
#' @section Imputed Missing Data:
#'
#' The SCF addresses item nonresponse using multiple imputation
#' (see Kennickell 1998). This procedure generates five completed data sets,
#' each containing distinct but plausible values for the missing entries. The
#' method applies a predictive model to the observed data, simulates variation
#' in both model parameters and residuals, and generates five independent
#' estimates for each missing value. These completed data sets—called
#' *implicates*—reflect both observed relationships and the uncertainty in
#' estimating them. See [scf_MIcombine()] for details.
#' 
#' @section Mock Data for Testing:
#' 
#' A mock SCF dataset (`scf2022_mock_raw.rds`) is bundled in `inst/extdata/` for 
#' internal testing purposes. It is a structurally valid `scf_mi_survey` object 
#' created by retaining only the first ~200 rows per implicate and only variables
#' used in examples and tests.
#' 
#' This object is intended solely for package development and documentation rendering. 
#' It is **not suitable for analytical use or valid statistical inference.**
#' 
#'
#' @section Theming and Visual Style:
#' All built-in graphics follow a common aesthetic set by [scf_theme()]. Users
#' may modify the default theme by calling `scf_theme()` explicitly within their
#' scripts. See [scf_theme()] documentation for customization options.
#'
#' @section Pedagogical Design:
#'
#' The package is designed to support instruction in advanced methods courses
#' on complex survey analysis and missing data. It promotes pedagogical
#' transparency through several features:
#'
#' - Each implicate’s design object is accessible via `scf_mi_survey$mi_design[[i]]`
#' - Raw implicate-level data can be viewed directly through `scf_mi_survey$mi_design[[i]]$variables`
#' - Users can execute analyses on individual implicates or combine them using Rubin’s Rules
#' - Key functions implement design-based estimation strategies explicitly, such as replicate-weight variance estimation
#' - Minimal abstraction is used, so each step remains visible and tractable
#'
#' These features allow instructors to demonstrate how survey weights, replicate
#' designs, and multiple imputation contribute to final results. Students can
#' follow the full analytic path from raw inputs to pooled estimates using
#' transparent, inspectable code and data structures.
#'
#' @references
#' Barnard J, Rubin DB. Small-sample degrees of freedom with multiple imputation.
#'   \doi{10.1093/biomet/86.4.948}.
#'
#' Bricker J, Henriques AM, Moore KB. Updates to the sampling of wealthy families in the Survey of Consumer Finances.
#'   Finance and Economics Discussion Series 2017-114.
#'   https://www.federalreserve.gov/econres/scfindex.htm
#'
#' Kennickell AB, McManus DA, Woodburn RL. Weighting design for the 1992 Survey of Consumer Finances.
#'   U.S. Federal Reserve.
#'   <https://www.federalreserve.gov/Pubs/OSS/oss2/papers/weight92.pdf>
#'
#' Kennickell AB. Multiple imputation in the Survey of Consumer Finances.
#'   Statistical Journal of the IAOS 33(1):143-151.
#'   \doi{10.3233/SJI-160278}.
#'
#' Little RJA, Rubin DB. Statistical analysis with missing data.
#'   ISBN: 9780470526798.
#'
#' Lumley T. survey: Analysis of complex survey samples. R package version 4.1-1.
#'   <https://CRAN.R-project.org/package=survey>
#'
#' Lumley T. Analysis of complex survey samples.
#'   \doi{10.18637/jss.v009.i08}.
#'
#' Lumley T. Complex surveys: A guide to analysis using R. ISBN: 9781118210932.
#'
#' U.S. Federal Reserve. Codebook for 2022 Survey of Consumer Finances.
#'   https://www.federalreserve.gov/econres/scfindex.htm
#'
#' @docType package
#' @name scf
"_PACKAGE"
