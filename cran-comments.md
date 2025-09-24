## Note to Reviewers

I am having difficulty juggling the following:
1. The need to use synthetic data objects to test functions. Using these 
  synthetic objects requires slightly different code than would be used in real 
  work.
2. The desire to include examples of how code would be implemented in reality
3. The prohibition on /donotrun{}
4. The need to download large data objects (1 Gb) from the Federal Reserve to 
  use the real functions using standard code to keep package size small.
5. The fact that /donottest{} requires a long time to run (> 30 minutes).

Are any of the following possible or advisable, given the long test times 
incurred by my real-world examples?
1. Can CRAN allow me to use /donotrun{} for my real-world examples?
2. Can CRAN tolerate a 30 minute+ test time for /donottest{} to run? Or
3. Should I get rid of real world examples in the roxygen code?

I would very much prefer 1 or 2 because I believe that it will render a better 
user experience, but either would require an exception to policy.  I can do #3 
if such an exception is too complicated.

Thank you for all your work.


## Test environments
* Local: R 4.3.1 on Windows 11 x86_64
* R-hub: windows-x86_64-devel, ubuntu-gcc-release

## R CMD check results
There were no ERRORs or WARNINGs.

## Additional comments
* A known benign warning ("non-integer #successes in a binomial glm!") may 
appear when using `svyglm(family = binomial())` with replicate weights. This 
does not affect results or inference. 
See: https://stackoverflow.com/questions/12953045

## Mock Data
The mock data is a truncated subset of the actual 2022 SCF public-use data. 
It includes all five implicates and replicate weights to enable valid demonstrations of 
multiply-imputed, replicate-weighted analysis.  It is a much reduced real-world
object to satisfy CRAN's requirement of a package size below 5Mb, which proved 
challenging given the 999 replicate weights.

It is necessary for CRAN-safe examples using scf_*() functions, which rely on 
the full SCF data structure. The mock data is used only in test blocks and is 
clearly labeled in documentation and examples as not representative.  This 
mock data generates warnings by design.

