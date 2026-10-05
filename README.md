SCF: An R Package for Analyzing the Survey of Consumer Finances
================

[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R
version](https://img.shields.io/badge/R-%3E%3D%203.6-blue.svg)](https://CRAN.R-project.org/)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/scf)](https://CRAN.R-project.org/package=scf)

## Overview

The `scf` R package analyzes the U.S. Federal Reserve's Survey of Consumer
Finances (SCF), a detailed source of data on U.S. households' wealth, debt,
and income.

It stores the SCF's five implicates and 999 replicate weights in one object
(`scf_mi_survey`) and provides functions for descriptive statistics,
tests, regression models, and plots. Standard errors follow the Fed's
method by default (see `?scf_variance`).

The SCF's unit is the primary economic unit: the person or couple at the
economic center of the household and everyone financially dependent on
them. The Fed calls this unit a "family"; this package calls it a
household.

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Getting Started](#getting-started)
- [Documentation](#documentation)

## Features

### Data Preparation

- `scf_download()`: Downloads and preprocesses SCF microdata, including
  all five implicates and 999 replicate weights.
- `scf_load()`: Loads `.rds` files into structured `scf_mi_survey`
  objects ready for analysis.
- `scf_update()`: Adds or transforms variables uniformly across all implicates.
- `scf_update_by_implicate()`: Applies a user-defined transformation to
  each implicate's data frame separately. Use when a computation depends
  on the within-implicate distribution (e.g., implicate-specific ranks
  or percentile thresholds).
- `scf_subset()`: Subsets the data consistently across all implicates.
- `scf_real()`: Converts raw `x` dollar variables to 2022 dollars, the way
  the Fed converts the summary variables.

### Descriptive Statistics

- `scf_freq()`: Weighted frequency tables for categorical variables.
- `scf_xtab()`: Cross-tabulations by row, column, or cell percentages.
- `scf_mean()`, `scf_median()`, `scf_percentile()`: Means, medians, and
  percentiles, overall or by group.
- `scf_ratio()`: Ratios of totals, such as wages as a share of income.
- `scf_pctile_sum()`: Creates percentile-based grouping variables for a
  continuous variable and optionally computes a summary statistic within
  each group. Supports an implicate-specific survey-weighted method
  (default) and the Federal Reserve's published stacking convention.
- `scf_corr()`: Weighted Pearson correlations.

### Statistical Inference

- `scf_ttest()`: One-sample and two-sample t-tests for continuous
  variables.
- `scf_prop_test()`: One-sample and two-sample proportion tests for
  binary variables.
- `scf_MIcombine()`: Combines model estimates across implicates using
  Rubin's rules.

### Regression Modeling

- `scf_ols()`: Linear regression with pooled estimates and implicate
  diagnostics.
- `scf_glm()`: Generalized linear models (e.g., logistic, Poisson).
- `scf_logit()`: Wrapper for logistic regression with optional odds
  ratio output.
- `scf_quantreg()`: Weighted quantile regression with pooled coefficients
  across implicates, one quantile per call.

All model functions return objects of class `scf_model_result`, with
methods for `coef()`, `vcov()`, `predict()`, `AIC()` (not for quantile
models), `residuals()`, and `summary()`.

### Visualization

- `scf_plot_dist()`: Bar charts of a variable's distribution.
- `scf_plot_dbar()`: Bar plots of categorical variable distributions.
- `scf_plot_bbar()`: Stacked bar plots for two categorical variables.
- `scf_plot_cbar()`: Bar plots for continuous variable summaries by
  group.
- `scf_plot_smooth()`: Smoothed line plots for continuous distributions.
- `scf_plot_hist()`: Weighted histograms of continuous variables.
- `scf_plot_hex()`: Weighted hexbin plots for bivariate continuous data.

### Diagnostics and Output

- `scf_nominal()`: Converts results from `scf_mean()`, `scf_median()`,
  `scf_percentile()`, and `scf_ttest()` from 2022 dollars to nominal dollars
  of the survey year, using the CPI-U-RS factors from the Fed's SCF Bulletin
  SAS macro. Formerly `scf_deflate()`.
- `scf_regtable()`: Regression tables from one or more model results, as
  console text, Markdown, LaTeX, or CSV.
- `scf_implicates()`: Implicate-level estimates from descriptive results and models.

## Installation

Install the latest version of the package through CRAN:

``` r
install.packages("scf")
```

The package requires R 3.6 or later and these packages, which install
with it:

- `survey` (replicate-weighted designs)
- `quantreg` (quantile regression)
- `ggplot2` (plots)
- `httr`, `haven` (downloading and reading SCF data)
- `stats`, `utils`

## Getting Started

### Download and Load Data

``` r
# Download SCF data for 2022:
scf_download(2022)

# Load the data into a survey design object:
scf2022 <- scf_load(2022)
```

### Explore and Summarize

#### Univariate Distributions

``` r
# Frequency of education categories
scf_freq(scf2022, ~edcl)

# Median household net worth
scf_median(scf2022, ~networth)

# 90th percentile of income
scf_percentile(scf2022, ~income, q = 0.9)

# Histogram of net worth distribution
scf_plot_hist(scf2022, ~networth)

# Smoothed density plot of income
scf_plot_smooth(scf2022, ~income)
```

#### Bivariate Relationships

``` r
# Cross-tabulation of education and homeownership
scf_xtab(scf2022, ~edcl, ~own)

# Stacked bar chart: homeownership by education
scf_plot_bbar(scf2022, ~edcl, ~own)

# Weighted bar chart: mean net worth by education
scf_plot_cbar(scf2022, ~networth, ~edcl, stat = "mean")

# Grouped median income by race
scf_median(scf2022, ~income, by = ~racecl)

# Correlation between income and net worth
scf_corr(scf2022, ~income, ~networth)

# Hexbin plot: income vs. net worth
scf_plot_hex(scf2022, ~income, ~networth)
```

### Statistical Testing

``` r
# One-sample proportion test: Is more than 10% of households rich?
scf_prop_test(scf2022, ~I(networth > 1e6), p = 0.10, alternative = "greater")

# Two-sample proportion test: Are men more likely than women to be rich?
# The estimate is the first group (Male) minus the second (Female).
scf_prop_test(scf2022, ~I(networth > 1e6), ~factor(hhsex, labels = c("Male", "Female")), alternative = "greater")

# One-sample t-test: Is mean income different from $75,000?
scf_ttest(scf2022, ~income, mu = 75000)

# Two-sample t-test: Are households with heads over 50 wealthier?
# The estimate is the first group (FALSE, 50 or under) minus the second (TRUE).
scf_ttest(scf2022, ~networth, ~I(age > 50), alternative = "less")
```

### Regression Modeling

``` r
# Linear regression: Predict net worth from income and education
scf_ols(scf2022, networth ~ income + factor(edcl))

# Generalized linear model: owning stocks, logistic regression
scf_glm(scf2022, hstocks ~ income + age + factor(edcl), family = binomial())

# Logit wrapper: owning stocks, reported as odds ratios
scf_logit(scf2022, hstocks ~ age + income + factor(edcl))
```

### Plotting and Visualization

``` r

# Bar chart of a single categorical variable
scf_plot_dbar(scf2022, ~edcl)

# Stacked bar chart comparing education by race
scf_plot_bbar(scf2022, ~edcl, ~racecl, scale = "percent", percent_by = "row")

# Smoothed line plot of net worth distribution
scf_plot_smooth(scf2022, ~networth, xlim = c(0, 2e6), method = "loess")

# Histogram of income distribution
scf_plot_hist(scf2022, ~income, bins = 40, xlim = c(0, 300000))

# Bar chart of mean net worth by education level
scf_plot_cbar(scf2022, ~networth, ~edcl, stat = "mean")

# Hexbin plot: net worth vs. income
scf_plot_hex(scf2022, ~income, ~networth, bins = 60)

```

### Wrangling and Transformation

``` r
# Create new variables across all implicates
scf2022 <- scf_update(scf2022,
  rich = networth > 1e6,
  senior = age >= 65,
  log_income = log(income + 1)
)

# Apply implicate-specific transformations (e.g., implicate-specific ranks)
scf2022 <- scf_update_by_implicate(scf2022, function(df) {
  threshold <- quantile(df$networth, probs = 0.90, na.rm = TRUE)
  df$top10nw <- df$networth >= threshold
  df
})

# Subset to working-age households with positive net worth
scf_sub <- scf_subset(scf2022, age >= 25 & age < 65 & networth > 0)

# Extract implicate-level estimates from a frequency table
freq <- scf_freq(scf_sub, ~own)
scf_implicates(freq, long = TRUE)
```

### Percentile Grouping

``` r
# Mean net worth by decile (implicate method)
scf_pctile_sum(scf2022, ~networth)

# Top 10% vs. bottom 90%, stack method (fast; replicates Fed convention)
scf_pctile_sum(scf2022, ~networth,
               probs  = c(0, 0.9, 1),
               labels = c("bottom90", "top10"),
               method = "stack")

# Add a percentile grouping variable to the object for use in other functions
scf2022 <- scf_pctile_sum(scf2022, ~networth,
                           probs = c(0, 0.5, 0.9, 1),
                           labels = c("bottom50", "next40", "top10"),
                           stat = "none")
scf_median(scf2022, ~income, by = ~networth_pctile)
```

### Real and Nominal Dollars

The SCF data mix two kinds of dollars. Summary variables made by the Fed,
such as `income`, `networth`, and `asset`, are in 2022 dollars in every
survey year. Raw survey variables, named `x` followed by a number, are in
dollars of the survey year. Do not compare raw variables across years, or
combine them with summary variables, until they are converted.

``` r
scf2016 <- scf_load(2016)

# Raw variables to 2022 dollars. Name income items, which are reported
# for the year before the survey, in `income`:
scf2016 <- scf_real(scf2016, ~x5702 + x3915, income = ~x5702)
scf_median(scf2016, ~x5702_real)

# Results from 2022 dollars to 2016 dollars:
m2016 <- scf_mean(scf2016, ~income)
scf_nominal(m2016)
```

Both functions stop with an error if given the wrong kind of variable,
unless `force = TRUE`.

### Quantile Regression

``` r
# Median regression: net worth on income and education
scf_quantreg(scf2022, networth ~ income + factor(edcl), tau = 0.5)

# Several quantiles: one call each
lapply(c(0.25, 0.5, 0.75, 0.9), function(t) {
  scf_quantreg(scf2022, networth ~ income + factor(edcl), tau = t)
})
```

### Regression Tables

``` r
# Compare OLS and logit models in a single formatted table
m_ols   <- scf_ols(scf2022, networth ~ income + factor(edcl))
m_logit <- scf_logit(scf2022, I(networth > 1e6) ~ income + factor(edcl))

scf_regtable(m_ols, m_logit)                          # console output
scf_regtable(m_ols, m_logit, output = "markdown")     # Markdown
scf_regtable(m_ols, m_logit, output = "latex")        # LaTeX
scf_regtable(m_ols, m_logit, output = "csv",
             file = "results/table1.csv")             # CSV file
```

## Documentation

For detailed examples, function documentation, and usage guides, consult
the package vignettes and reference manual.

- [SCF Homepage](https://github.com/jncohen/scf)
- [CRAN Package Page](https://CRAN.R-project.org/package=scf)

## Note on Mock Data

This package includes a small mock data set (`scf2022_mock_raw.rds`) for
examples and tests. It keeps 200 rows per implicate and a few variables. It
has the same structure as the real data but is not suitable for analysis.
`scf_load()` prints a message when it loads it.


## Citation

If you use `scf` in published work, please cite it as:

> Joseph N. Cohen (2026). *scf: Analyzing the Survey of Consumer Finances.* R package version 1.1.0. <https://github.com/jncohen/scf>

Use `citation("scf")` in R for formatted references.

## Use of AI Tools

Development of version 1.1.0 used Claude (Anthropic), an AI assistant, to
help write and revise code, tests, and documentation; to audit functions for
errors; and to run validation checks. The author directed this work, made all
methodological decisions, and reviewed the changes. The package's results were
checked against the Federal Reserve's published SCF Bulletin tables (Tables
1-5, 1989-2022) and against independent calculations with the `survey` and
`mitools` packages.

## Author

Joseph N. Cohen  
Department of Sociology & Program in Data Analytics  
Queens College, City University of New York  
<joseph.cohen@qc.cuny.edu>
