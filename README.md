
# catfun

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/catfun)](https://cran.r-project.org/package=catfun)
[![Build
Status](https://travis-ci.org/nt-williams/catfun.svg?branch=master)](https://travis-ci.org/nt-williams/catfun)

catfun simplifies existing functions for categorical data analysis.

## Installation

``` r
devtools::install_github("nt-williams/catfun")
```

## What’s inside

catfun includes wrapper functions for hypothesis tests of proportions,
power analyses for proportions, and funciton for calculating risk
difference from a 2 x 2 table.

For example, `prop_test()` combines the base R functions `binom.test()`,
`prop.test()` and `BinomCI()` from the *DescTools* package into one. The
3 mentioned functions all perform important aspects of testing for
differences among proportions but are now used to create one output with
all relevant information within one spot.
