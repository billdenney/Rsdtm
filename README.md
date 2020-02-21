# Rsdtm

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/billdenney/Rsdtm.svg?branch=master)](https://travis-ci.org/billdenney/Rsdtm)
[![Codecov test coverage](https://codecov.io/gh/billdenney/Rsdtm/branch/master/graph/badge.svg)](https://codecov.io/gh/billdenney/Rsdtm?branch=master)
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
