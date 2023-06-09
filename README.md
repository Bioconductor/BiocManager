
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BiocManager

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/BiocManager)](https://cran.r-project.org/package=BiocManager)
[![CRAN
release](http://www.r-pkg.org/badges/version-last-release/BiocManager)](https://github.com/Bioconductor/BiocManager/releases)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/BiocManager)](https://cran.r-project.org/package=BiocManager)
<!-- badges: end -->

## Overview

The `BiocManager` package, as the modern successor package to
`BiocInstaller`, allows users to install and manage packages from the
*[Bioconductor](https://bioconductor.org)* project. Bioconductor focuses
on the statistical analysis and comprehension of high-throughput genomic
data.

Current *Bioconductor* packages are available on a ‘release’ version
intended for every-day use, and a ‘devel’ version where new features are
continually introduced. A new release version is created every six
months. Using the `BiocManager` package helps users accurately install
packages from the appropriate release.

-   `available()` shows all packages associated with a search pattern
-   `install()` installs and/or updates packages either CRAN or
    Bioconductor
-   `repositories()` shows all package repository URL endpoints
-   `valid()` checks and returns packages that are out-of-date or too
    new
-   `version()` returns the current Bioconductor version number

## Installation

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
```

## Usage

### Checking Bioconductor version currently installed

``` r
BiocManager::version()
#> [1] '3.15'
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

Please see the package vignette for more detailed information such as
changing Bioconductor version, offline use, and other advanced usage.

## Getting help

To report apparent bugs, create a minimal and reproducible example on
[GitHub](https://github.com/Bioconductor/BiocManager/issues).
