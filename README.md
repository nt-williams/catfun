
# catfun <img src="man\figures\catfun_final.png" align="right" height = "150" />

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/catfun)](https://cran.r-project.org/package=catfun)
[![Build
Status](https://travis-ci.org/nt-williams/catfun.svg?branch=master)](https://travis-ci.org/nt-williams/catfun)

catfun simplifies existing functions for categorical data analysis.

## Installation

catfun can be installed from CRAN using:

``` r
install.packages("catfun")
```

The development version can be installed from github using:

``` r
devtools::install_github("nt-williams/catfun")
```

## What’s inside

R currently supports a wide variety of tools for the analysis of
categorical data. However, many functions are spread across a variety of
packages with differing syntax and poor compatibility with each another.

To address this issue, *catfun* includes wrapper functions around
existing functions for tests of proportions, power analyses for
proportions, and measures of association into one structure.

For example, `prop_test()` combines the base R functions `binom.test()`,
`prop.test()` and `BinomCI()` from the *DescTools* package into one. The
3 mentioned functions all perform important aspects of testing for
differences among proportions but are now used to create one output with
all relevant information within one spot.
