## Test environments
* Local: R 4.3.1 on Windows 11 x86_64
* R-hub: windows-x86_64-devel, ubuntu-gcc-release

## R CMD check results
There were no ERRORs.
There were no WARNINGs of concern.
There was 1 NOTE:
  * This is the initial submission of the package.

## Additional comments
* A known benign warning ("non-integer #successes in a binomial glm!") may appear when using `svyglm(family = binomial())` with replicate weights. This does not affect results or inference. See: https://stackoverflow.com/questions/12953045

## Package Size Justification
The installed package size exceeds 5MB (~20MB) due to a bundled file, mock_scf2022.rds, located in inst/extdata/.

This file is a truncated subset (N = 200) of the actual 2022 SCF public-use data. It includes all five implicates and replicate weights to enable valid demonstrations of multiply-imputed, replicate-weighted analysis.

It is necessary for CRAN-safe examples using scf_*() functions, which rely on the full SCF data structure. The mock data is used only in \donttest{} blocks and is clearly labeled in documentation and examples as not representative.
