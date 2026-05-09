# Contributing to scf

Thank you for your interest in contributing to the `scf` package.

## Reporting issues

Please use the [GitHub issue tracker](https://github.com/jncohen/scf/issues) to report bugs or request features. When reporting a bug, include:

- A minimal reproducible example
- Your R version and operating system
- The output of `sessionInfo()`

Note that `scf` functions require SCF microdata downloaded via `scf_download()`. The bundled mock dataset (`scf2022_mock_raw.rds`) is provided for testing package structure only and is not suitable for analytical use.

## Pull requests

Contributions via pull request are welcome. Please:

1. Fork the repository and create a feature branch from `main`
2. Follow existing code style (base R, no tidyverse dependencies in core functions)
3. Add or update documentation using roxygen2 comments
4. Add tests in `tests/testthat/` using the mock data where possible
5. Run `devtools::check()` and resolve all ERRORs and WARNINGs before submitting

## Code style

- Functions follow the `scf_` prefix convention
- All exported functions must have complete roxygen2 documentation including `@examples`
- Examples must run on the bundled mock data or be wrapped in `\dontrun{}`

## Contact

Joseph N. Cohen — joseph.cohen@qc.cuny.edu  
Department of Sociology, Queens College, CUNY
