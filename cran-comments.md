## Release summary
This is a minor release (1.0.10 to 1.1.0). It corrects standard errors and
other results across the package and adds new features.

Corrections:
* Standard errors now follow the Fed's replicate-weight method. Earlier
  versions put the main and replicate weights on different scales and
  overstated standard errors.
* Fixes to `scf_xtab`, `scf_corr`, `scf_prop_test`, `scf_ttest`,
  `scf_quantreg`, `scf_regtable`, `scf_pctile_sum`, `scf_deflate`, several
  plots, `confint()`, and AIC reporting.
* Functions stop with a clear error when a variable has missing values, or
  when a model or estimate fails in any implicate, instead of pooling the
  rest.

New features:
* `scf_ratio()` estimates ratios of totals.
* `scf_real()` converts raw SCF dollar variables to 2022 dollars, following
  the Fed's SAS macro.
* A `variance` option chooses the Fed's method (default) or Rubin's rules;
  see `?scf_variance`.

Changes that may affect user code:
* The first argument of `scf_update()` is renamed `.object`.
* `scf_deflate()` is renamed `scf_nominal()`; `scf_deflate()` still works
  with a deprecation warning.
* `scf_prop_test()`, `scf_glm()`, and `scf_logit()` report t and Rubin's
  degrees of freedom instead of z; the `z.value` column is now `t.value`.
* `scf_quantreg()` uses replicate-weight standard errors by default and no
  longer reports AIC.
* `scf_regtable()` chooses decimal places by size unless `digits` is given.
* `scf_pctile_cut()`, renamed `scf_pctile_sum()` in 1.0.7, now gives a
  deprecation warning.
* The `rlang` dependency is dropped.

## Test environments
* Local: R 4.5.1 on Windows 11 x86_64
* Win-builder: R-devel (2026-09-30 r90605)
* Win-builder: R-release (R 4.6.1)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Reverse dependencies
There are no reverse dependencies on CRAN.

## Mock data
This package includes a small mock data set (`inst/extdata/scf2022_mock_raw.rds`)
for examples and tests. The full data are too large for CRAN's 5 MB limit.
The mock set is a 200-row subset of each of the five implicates of the 2022
SCF public-use data, with all 999 replicate weights. It has the same
structure as the full data, so the functions run as intended.

Because the mock data are small, some model examples may produce
convergence warnings. This does not reflect problems with the full SCF data.
Tests that refit models with all 999 replicate weights are skipped on CRAN
to keep check time down.
