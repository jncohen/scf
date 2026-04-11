---
title: "scf: An R package for analysis of Survey of Consumer Finances public-use microdata"
tags:
  - R
  - survey methodology
  - household finance
  - Survey of Consumer Finances
  - multiple imputation
  - complex surveys
authors:
  - name: Joseph N. Cohen
    orcid: 0000-0002-6197-4453
    affiliation: 1
affiliations:
  - name: Queens College, City University of New York, Queens, New York, USA
    index: 1
date: 21 March 2026
bibliography: paper.bib
---

# Summary

The *Survey of Consumer Finances* (SCF) is a triennial national survey
of U.S. households’ income, assets, debts, financial behavior, and
household organization (U.S. Federal Reserve Bank 2023). The data are
used in academic research, Congressional research reports, and the
production of official statistics.

The R package `scf` (Cohen 2025) provides a unified framework for
acquiring, preparing, analyzing, and visualizing SCF public-use
microdata. It encodes standard practices for handling the survey’s
design, automates implicate-level estimation and pooling, and reduces
implementation complexity for common analytical tasks.

Cohen (2026) offers a more detailed description of the package design
and validates its ability to produce official statistics.

# Statement of Need

The valid analysis of SCF microdata requires a multi-step workflow that
combines replicate-weighted survey estimation with the handling of
multiply imputed datasets. Analysts must construct replicate-weighted
survey designs for each implicate, estimate quantities separately across
implicates, and combine results using appropriate pooling rules.

In R, this workflow is typically implemented using the `survey` package
(Lumley 2011) along with imputed data handling tools like `mitools`
(Lumley 2019) or custom scripts. These tools are flexible and
methodologically appropriate but require users to manually coordinate
design specification, implicate iteration, and pooling. As a result,
even simple analyses involve substantial custom code, and correct
implementation depends on careful adherence to SCF-specific conventions.
Existing reference workflows, such as those provided by Damico (Damico
2026), demonstrate correct implementation but require dozens of lines of
code that can slow or discourage non-specialists’ engagement of the
data.

The `scf` package addresses this gap by formalizing SCF-specific best
practices in a standardized functional interface. It provides a unified
data object that encodes the replicate-weighted design across
implicates, along with a set of functions that automate estimation and
pooling. The package provides a consistent grammar and output format
that reduces the amount of custom code required for common tasks. It
reproduces official SCF statistics and reduces typical workflows from
dozens of lines of custom code to a small number of standardized
function calls (Cohen 2026).

# Package overview

The package is designed around three principles: rigor, accessibility,
and transparency. It encodes SCF-specific survey and imputation
workflows in a structured object while preserving access to
implicate-level survey designs for advanced use.

## Core object

The central object in the package is `scf_mi_survey`, a structured
container representing one SCF wave. It stores the five imputed data
sets with an associated `survey::svrepdesign` object, along with
supplemental information.

## Estimation workflow

Most `scf` functions follow the same internal pattern:

1.  construct or retrieve the implicate-specific survey designs
2.  apply the corresponding estimation routine to each implicate
3.  combine results into a pooled estimate and associated uncertainty
    measure
4.  store the combined results and results from individual implicates in
    an R list

For model-based and many descriptive quantities, pooling uses
Rubin-style combining rules (Rubin 1987). For some statistics such as
percentiles and proportions, the package uses appropriate
implicate-aggregation strategies implemented per official documentation.
The result is a consistent user interface across common SCF tasks
without requiring users to manage the imputation and replicate-weight
logic manually.

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
infrastructure for data handling and graphics.

The package is available on CRAN:

    install.packages("scf")

Source code, examples, and documentation are available at:
<https://github.com/jncohen/scf>. The package is distributed under the
MIT License.

# Acknowledgements

The package builds on the `survey` package and on publicly available SCF
documentation and example workflows.

# AI Use Declaration

Generative AI tools were used as aids in coding, debugging, and drafting
portions of the documentation and manuscript. Their use occurred within
a human-directed workflow in which the author determined all substantive
content and structure. AI-generated outputs were treated as provisional
and subject to review and revision. The conceptual design, methodology,
and implementation were developed by the author. The author has reviewed
and validated all material and retains full responsibility for the work.

Cohen, Joseph N. 2025. *Scf: Analyzing the Survey of Consumer Finances*.
V. 1.0.5. Released November 20.
<https://cran.r-project.org/web/packages/scf/index.html>.

Cohen, Joseph N. 2026. *Analyzing the Survey of Consumer Finances with
‘Scf‘*. OSF. [osf.io/azrsn](https://osf.io/azrsn).

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
