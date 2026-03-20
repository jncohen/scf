# Summary

The *Survey of Consumer Finances* (SCF) is a triennial national survey
of U.S. household finances and a widely used source of microdata for
analyses of income, assets, debts, financial behavior, and household
organization (U.S. Federal Reserve Bank 2023). The data are used in
academic research, Congressional research reports, and the production of
official statistics.

SCF microdata incorporate features that improve inferential quality but
complicate analysis, including replicate-weighted sampling and the use
of multiple imputation to handle missing values. These features require
specialized workflows to obtain valid estimates and uncertainty
measures.

The R package `scf` (**Cohen2025?**) provides a unified framework for
acquiring, preparing, analyzing, and visualizing SCF public-use
microdata. It encodes standard practices for handling the survey’s
design, automates implicate-level estimation and pooling, and reduces
implementation complexity for common analytical tasks.

# Statement of Need

The valid analysis of SCF microdata requires a multi-step workflow that
combines replicate-weighted survey estimation with multiple imputation.
Analysts must construct replicate-weighted survey designs for each
implicate, estimate quantities separately across implicates, and combine
results using appropriate pooling rules.

In R, this workflow is typically implemented using the `survey` package
(Lumley 2011) along with imputed data handling tools like `mitools`
(Lumley 2019) or custom scripts. These tools are flexible and
methodologically appropriate but require users to manually coordinate
design specification, implicate iteration, and pooling. As a result,
even simple analyses involve substantial custom code, and correct
implementation depends on careful adherence to SCF-specific conventions.

Existing reference workflows, such as those provided by Damico (Damico
2026), demonstrate correct implementation but are not packaged as
reusable abstractions. Users must adapt and reimplement these procedures
for each analysis task.

The `scf` package addresses this gap by formalizing SCF-specific best
practices in a standardized functional interface. It provides a unified
data object that encodes the replicate-weighted design across
implicates, along with a set of functions that automate estimation and
pooling. The package attempts a grammar and output format that reduces
that amount of custom code required for common tasks, reducing the
barrier to entry for valid SCF analysis while preserving transparency
and flexibility for advanced users.

# Package overview

## Core object

The central object in the package is `scf_mi_survey`, a structured
container representing one SCF wave. It stores:

- `mi_design`: a list of five `survey::svrepdesign` objects, one for
  each implicate
- `year`: the survey year
- `n_households`: the number of households represented by the survey
  wave

This design supports two goals simultaneously. First, it allows the
package to automate repeated operations across implicates. Second, it
preserves transparency by allowing users to inspect or extract
implicate-level survey objects directly for advanced diagnostics and
custom modeling.

## Estimation workflow

Most `scf` functions follow the same internal pattern:

1.  construct or retrieve the implicate-specific survey designs  
2.  apply the corresponding estimation routine to each implicate  
3.  combine results into a pooled estimate and associated uncertainty
    measure

For model-based and many descriptive quantities, pooling uses
Rubin-style combining rules (Rubin 1987). For some statistics such as
percentiles and proportions, the package uses appropriate
implicate-aggregation strategies implemented in the workflow. The result
is a consistent user interface across common SCF tasks without requiring
users to manage the imputation and replicate-weight logic manually.

## Functional coverage

The package supports end-to-end SCF workflows through functions for:

- data acquisition and loading (`scf_download()`, `scf_load()`,
  `scf_design()`)  
- data wrangling (`scf_update()`, `scf_subset()`)  
- descriptive estimation (`scf_mean()`, `scf_median()`,
  `scf_percentile()`, `scf_freq()`, `scf_xtab()`, `scf_corr()`)  
- inference (`scf_ttest()`, `scf_prop_test()`)  
- modeling (`scf_ols()`, `scf_logit()`, `scf_glm()`)  
- results extraction and formatting (`scf_MIcombine()`,
  `scf_implicates()`, `scf_regtable()`)  
- visualization (`scf_plot_dbar()`, `scf_plot_cbar()`,
  `scf_plot_bbar()`, `scf_plot_dist()`, `scf_plot_hist()`,
  `scf_plot_hex()`, `scf_plot_smooth()`, `scf_theme()`)

These functions reduce repetition in common SCF workflows while
remaining compatible with the broader `survey` ecosystem.

# Example

A typical workflow is concise:

    library(scf)

    scf_download(2022)
    scf2022 <- scf_load(2022)

    scf_mean(scf2022, ~income)

Users can also transform variables and fit models within the same object
framework:

    scf2022 <- scf_update(scf2022, senior = age >= 65)

    fit <- scf_ols(scf2022, log(networth + 1) ~ senior)
    scf_regtable(fit, digits = 3, output = "console")

This workflow delegates design specification, implicate iteration, and
pooling to the package while leaving the substantive modeling decisions
to the analyst.

# Availability

`scf` is implemented entirely in R. It builds primarily on the `survey`
package for complex-survey estimation and uses established CRAN
infrastructure for data handling and graphics. The package is available
on CRAN:

    install.packages("scf")

Source code, examples, and package documentation are available in the
associated repository.

# Acknowledgements

The package builds on the `survey` package and on publicly available SCF
documentation and example workflows.

# References

Damico, Anthony. 2026. *Ajdamico/Asdfree*. Released March 16.
<https://github.com/ajdamico/asdfree>.

Lumley, Thomas. 2011. *Complex Surveys: A Guide to Analysis Using R*.
John Wiley & Sons.

Lumley, Thomas. 2019. *Mitools: Tools for Multiple Imputation of Missing
Data*. V. 2.4. Released April 26.
<https://cran.r-project.org/web/packages/mitools/index.html>.

Rubin, Donald B. 1987. *Multiple Imputation for Nonresponse in Surveys*.
John Wiley & Sons.

U.S. Federal Reserve Bank. 2023. “Survey of Consumer Finances.” Survey
Data. <https://doi.org/10.17016/8799>.
