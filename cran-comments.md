## Test environments
* Local: R 4.3.1 on Windows 11 x86_64
* R-hub: windows-x86_64-devel, ubuntu-gcc-release

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE about installed package size:
* installed size is 8.9Mb
  sub-directories of 1Mb or more:
    extdata  8.5Mb
This NOTE is due to inclusion of a bundled mock SCF dataset required for CRAN-safe examples. 
The mock is a truncated subset of the 2022 SCF with all five implicates and replicate weights 
so that examples of multiply-imputed, replicate-weighted analysis can run quickly on CRAN. 
It is not representative of the full SCF.


## Additional comments
* A known benign warning ("non-integer #successes in a binomial glm!") may 
appear when using `svyglm(family = binomial())` with replicate weights. This 
does not affect results or inference. 
See: https://stackoverflow.com/questions/12953045

## Mock Data
The mock data is a truncated subset of the actual 2022 SCF public-use data. 
It includes all five implicates and replicate weights to enable valid demonstrations of 
multiply-imputed, replicate-weighted analysis.

It is necessary for CRAN-safe examples using scf_*() functions, which rely on 
the full SCF data structure. The mock data is used only in test blocks and is 
clearly labeled in documentation and examples as not representative.  This 
mock data generates warnings by design.

## 
