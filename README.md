
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BiocManager <a href="https://bioconductor.github.io/BiocManager/"><img src="man/figures/BiocManager.png" align="right" height="138" alt="BiocManager CRAN landing page"></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/BiocManager)](https://cran.r-project.org/package=BiocManager)
[![CRAN
release](http://www.r-pkg.org/badges/version-last-release/BiocManager)](https://github.com/Bioconductor/BiocManager/releases)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/BiocManager)](https://cran.r-project.org/package=BiocManager)
<!-- badges: end -->

## Overview

The `BiocManager` package allows users to install and manage packages
from the *[Bioconductor](https://bioconductor.org)* project, including
CRAN packages that depend or import Bioconductor packages. Bioconductor
focuses on the statistical analysis and comprehension of high-throughput
genomic data.

Current *Bioconductor* packages are available on a ‘release’ version
intended for every-day use, and a ‘devel’ version where new features are
continually introduced. A new release version is created every six
months. Using the `BiocManager` package helps users accurately install
packages from the appropriate release.

- `available()` shows all packages associated with a search pattern
- `install()` installs and/or updates packages either CRAN or
  Bioconductor
- `repositories()` shows all package repository URL endpoints
- `valid()` checks and returns packages that are out-of-date or too new
- `version()` returns the current Bioconductor version number

## Installation

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager", repos = "https://cran.R-project.org")
```

## Usage

### Checking Bioconductor version currently in use

``` r
BiocManager::version()
#> [1] '3.21'
```

### Installing Bioconductor packages

``` r
BiocManager::install(c("GenomicRanges", "SummarizedExperiment"))
```

### Verifying a valid Bioconductor installation

``` r
BiocManager::valid()
#> [1] TRUE
```

## More information

Please see the ‘Get started’ document (package vignette) for more
detailed information such as changing Bioconductor version, offline use,
and other advanced usage.

## Getting help

To report apparent bugs, create a minimal and reproducible example on
[GitHub](https://github.com/Bioconductor/BiocManager/issues).
