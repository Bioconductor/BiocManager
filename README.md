
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BiocManager

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/BiocManager)](https://cran.r-project.org/package=BiocManager)
[![CRAN
release](http://www.r-pkg.org/badges/version-last-release/BiocManager)](https://github.com/Bioconductor/BiocManager/releases)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/BiocManager)](https://cran.r-project.org/package=BiocManager)
<br> [![Travis build
status](https://travis-ci.org/Bioconductor/BiocManager.svg?branch=master)](https://travis-ci.org/Bioconductor/BiocManager)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/Bioconductor/BiocManager?branch=master&svg=true)](https://ci.appveyor.com/project/Bioconductor/BiocManager)
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

  - `available()` shows all packages associated with a search pattern
  - `install()` installs and/or updates packages either CRAN or
    Bioconductor
  - `repositories()` shows all package repository URL endpoints
  - `valid()` checks and returns packages that are out-of-date or too
    new
  - `version()` returns the current Bioconductor version number

## Installation

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
```

## Usage

### Checking Bioconductor version currently installed

``` r
BiocManager::version()
#> [1] '3.11'
```

### Installing Bioconductor packages

``` r
BiocManager::install(c("GenomicRanges", "SummarizedExperiment"))
```

### Verifying a valid Bioconductor installation

``` r
BiocManager::valid()
#> Warning: 0 packages out-of-date; 1 packages too new
#> 
#> * sessionInfo()
#> 
#> R Under development (unstable) (2019-10-07 r77258)
#> Platform: x86_64-pc-linux-gnu (64-bit)
#> Running under: Ubuntu 18.04.3 LTS
#> 
#> Matrix products: default
#> BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/libopenblasp-r0.2.20.so
#> 
#> locale:
#>  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
#>  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
#>  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
#>  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
#>  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
#> [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] BiocManager_1.30.9 compiler_4.0.0     magrittr_1.5      
#>  [4] tools_4.0.0        htmltools_0.4.0    yaml_2.2.0        
#>  [7] Rcpp_1.0.2         stringi_1.4.3      rmarkdown_1.16    
#> [10] knitr_1.25         stringr_1.4.0      xfun_0.10         
#> [13] digest_0.6.22      rlang_0.4.1        evaluate_0.14     
#> 
#> Bioconductor version '3.11'
#> 
#>   * 0 packages out-of-date
#>   * 1 packages too new
#> 
#> create a valid installation with
#> 
#>   BiocManager::install("rapiclient", update = TRUE, ask = FALSE)
#> 
#> more details: BiocManager::valid()$too_new, BiocManager::valid()$out_of_date
```

## More information

Please see the package vignette for more detailed information such as
changing Bioconductor version, offline use, and other advanced usage.

## Getting help

To report apparent bugs, create a minimal and reproducible example on
[github](https://github.com/Bioconductor/BiocManager/issues).
