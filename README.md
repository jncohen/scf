SCF: An R Package for Analyzing the Survey of Consumer Finances
================

[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R
version](https://img.shields.io/badge/R-%3E%3D%203.6-blue.svg)](https://cran.r-project.org/)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)

## Overview

The `scf` R package provides a structured, reproducible, and
pedagogically-aware toolkit for analyzing the U.S. Federal Reserve’s
**Survey of Consumer Finances (SCF)**, one of the highest-quality data
sources for information on U.S. households’ balance sheets and income
statements.

It wraps replicate-weighted, multiply-imputed SCF data into a consistent
object class (`scf_mi_survey`) and offers end-to-end support for
weighted descriptive statistics, hypothesis testing, regression
modeling, and high-quality visualizations—while transparently
incorporating Rubin’s Rules and complex sample design.

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
- `scf_update()`: Adds or transforms variables across implicates.
- `scf_subset()`: Subsets the data consistently across all implicates.

### Descriptive Statistics

- `scf_freq()`: Weighted frequency tables for categorical variables.
- `scf_xtab()`: Cross-tabulations by row, column, or cell percentages.
- `scf_mean()`, `scf_median()`, `scf_percentile()`: Computes groupwise
  or overall statistics using Rubin’s Rules.
- `scf_corr()`: Weighted Pearson correlations.

### Statistical Inference

- `scf_ttest()`: One-sample and two-sample t-tests for continuous
  variables.
- `scf_prop_test()`: One-sample and two-sample proportion tests for
  binary variables.
- `scf_MIcombine()`: Combines estimates across imputations using Rubin’s
  Rules (internal to most functions).

### Regression Modeling

- `scf_ols()`: Linear regression with pooled estimates and implicate
  diagnostics.
- `scf_glm()`: Generalized linear models (e.g., logistic, Poisson).
- `scf_logit()`: Wrapper for logistic regression with optional odds
  ratio output.

### Visualization

- `scf_plot_dist()`: Kernel density plots for visualizing and comparing distributions by group.
- `scf_plot_dbar()`: Bar plots of categorical variable distributions.
- `scf_plot_bbar()`: Stacked bar plots for two categorical variables.
- `scf_plot_cbar()`: Bar plots for continuous variable summaries by
  group.
- `scf_plot_smooth()`: Smoothed line plots for continuous distributions.
- `scf_plot_hist()`: Weighted histograms of continuous variables.
- `scf_plot_hex()`: Weighted hexbin plots for bivariate continuous data.

### Diagnostics and Output

- `scf_implicates()`: Extracts implicate-level results from SCF objects.
- `print()`, `summary()`: Custom methods for clean, interpretable output
  in analysis and teaching.

## Installation

The `scf` package is not yet on CRAN. To install the development version
from GitHub:

``` r
# Install devtools if you don't already have it
install.packages("devtools")

# Install the SCF package from GitHub
devtools::install_github("jncohen/scf")
```

The package requires **R ≥ 3.6** and the following packages:

- `survey` (for replicate-weighted designs)
- `ggplot2` (for plotting)
- `httr`, `haven` (for downloading and reading SCF data)
- `mitools`, `stats`, `utils`, `methods`, and others (loaded
  automatically)

Use `install.packages()` to install any missing dependencies manually if
needed.

## Getting Started

### Download and Load Data

``` r, eval = F
# Download SCF data for 2022:
scf_download(2022)

# Load the data into a survey design object:
scf2022 <- scf_load(2022)
```

```r
# Using mock data for CRAN compliance
scf2022 <- readRDS(system.file("extdata", "mock_scf2022.rds", package = "scf"))
# NOTE: This is mock data for demonstration only. 
# Use `scf_download()` and `scf_load()` for full SCF datasets.
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

# Two-sample proportion test: Are women less likely to be rich?
scf_prop_test(scf2022, ~I(networth > 1e6), ~factor(hhsex, labels = c("Male", "Female")), alternative = "less")

# One-sample t-test: Is mean income different from $75,000?
scf_ttest(scf2022, ~income, mu = 75000)

# Two-sample t-test: Are older households wealthier?
scf_ttest(scf2022, ~networth, ~I(age > 50), alternative = "greater")
```

### Regression Modeling

``` r
# Linear regression: Predict net worth from income and education
scf_ols(scf2022, networth ~ income + factor(edcl))

# Generalized linear model: Predict borrowing with logistic regression
scf_glm(scf2022, hborrff ~ income + age + factor(edcl), family = binomial())

# Logit wrapper: Predict probability of owning stocks
scf_logit(scf2022, ~I(owns_stocks == 1) ~ age + income + factor(edcl))
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

# Subset to working-age households with positive net worth
scf_sub <- scf_subset(scf2022, age >= 25 & age < 65 & networth > 0)

# Extract implicate-level estimates from a frequency table
freq <- scf_freq(scf_sub, ~own)
scf_implicates(freq, long = TRUE)
```

## Documentation:

For detailed examples, function documentation, and usage guides, consult
the package vignettes and reference manual.

- [SCF Homepage](https://github.com/jncohen/scf)
- [**Reference Manual:** Click here](https://github.com/jncohen/scf/blob/v1.0/scf_1.0.4.pdf)

## Note on Mock Data

This package includes a small mock dataset (`mock_scf2022.rds`) for testing purposes.  
It includes only 75 rows and select variables. It is structurally valid,  
but **not suitable for analytical use or inference**.


## Citation

If you use `scf` in published work, please cite it as:

> Joseph N. Cohen (2025). *scf: Tools for Analyzing the Survey of Consumer Finances.* R package. ver. 1.0.3. <https://github.com/jncohen/scf>

Use `citation("scf")` in R for formatted references.

## Author

Joseph N. Cohen  
Department of Sociology & Program in Data Analytics  
Queens College, City University of New York  
<joseph.cohen@qc.cuny.edu>
<https://jncohen.commons.gc.cuny.edu>
