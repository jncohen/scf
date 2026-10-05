# Contributing to scf

Fellow household finance enthusiasts! Thank you for your interest in contributing to the `scf` package. 

## Reporting issues

Please use the [GitHub issue tracker](https://github.com/jncohen/scf/issues) to report bugs or request features. Please include:

- A minimal reproducible example
- Your R version and operating system
- The output of `sessionInfo()`

Note that `scf` functions require SCF microdata downloaded via `scf_download()`. The bundled mock dataset (`scf2022_mock_raw.rds`) is provided for testing package while adhering to CRAN limits to package size.  Do not use the mock data for real analysis.

## Pull requests

Contributions or customizations via pull request are welcome. Do the following:

1. Fork the repository and create a feature branch from `main`
2. Follow existing code style (base R, no tidyverse dependencies in core functions)
3. Add or update documentation using roxygen2 comments
4. Add tests in `tests/testthat/` using the mock data where possible
5. Run `devtools::check()` and resolve all ERRORs and WARNINGs before submitting

## Contact

Joseph N. Cohen <joseph.cohen@qc.cuny.edu>
Department of Sociology, CUNY Queens College
