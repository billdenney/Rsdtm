# Rsdtm

<!-- badges: start -->
[![R-CMD-check](https://github.com/billdenney/Rsdtm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/billdenney/Rsdtm/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/billdenney/Rsdtm/branch/main/graph/badge.svg)](https://app.codecov.io/gh/billdenney/Rsdtm?branch=main)
[![CRAN status](https://www.r-pkg.org/badges/version/Rsdtm)](https://CRAN.R-project.org/package=Rsdtm)
<!-- badges: end -->

The goal of Rsdtm is to simplify SDTM data management and file creation for R.

## Installation

### GitHub

To install the development version, run the following:

``` r
devtools::install_github("billdenney/Rsdtm")
```

### CRAN (someday)

When it is on CRAN (someday), you can install the released version of Rsdtm from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("Rsdtm")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Rsdtm)
d <- import_sdtm("/director/to/data")
```
