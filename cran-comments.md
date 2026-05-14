
## Test environments
* Local: R 4.3.1 on Windows 11 x86_64
* R-hub: windows-x86_64-devel, ubuntu-gcc-release
* Win-builder: R-devel, R-release

## R CMD check results
There were no ERRORs or WARNINGs.

## Notes
Source files use Windows (CRLF) line endings, as the package is developed on
Windows. This is a pre-existing condition accepted in prior CRAN releases.

## Warnings with scf_logit() and scf_glm()
A known benign warning ("non-integer #successes in a binomial glm!") may 
appear when using `svyglm(family = binomial())` with replicate weights. This 
does not affect results or inference. 
See: https://stackoverflow.com/questions/12953045

## Mock Data

This package includes a small mock dataset (`mock_scf2022.rds`) for testing 
purposes. The size and complexity of this data make it difficult to produce a
tarball under CRAN's 5Mb limit. The mock set is a truncated subset of the 
actual 2022 SCF public-use data, which includes the first 75 observations of 
the set, recorded in a complex data object that includes all five implicates, 
each with 999 replicate weights. It preserves the structure of the full data, 
allowing the `scf` functions to operate as intended. 

Because the mock dataset is small by design, certain functions (especially 
`scf_logit()` on rare outcomes) may produce warnings or fail, but this does 
not reflect problems in the real-SCF workflow.

## Fast Update 1.0.8 after 1.0.7

This is a maintenance release submitted shortly after the previous release to fix
package metadata/build configuration and improve CRAN-readiness. A regrettable
oversight caused 1.0.7 to ship with an unpushed, highly functional change that
involves no changes to the user interface. My apologies to the hard-working 
people at CRAN.
