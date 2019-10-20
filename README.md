
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

  - `available()` shows all packages associated with a search pattern
  - `install()` installs and/or updates packages either CRAN or
    Bioconductor
  - `repositories()` shows all package repository URL endpoints
  - `valid()` checks and returns packages that are out-of-date or too
    new
  - `version()` returns the current Bioconductor version number

## Installation

``` r
install.packages("BiocManager")
```

## Usage

``` r
BiocManager::version()
#> [1] '3.8'

BiocManager::install(c("GenomicRanges", "SummarizedExperiment"))

BiocManager::valid()
#> Warning: 0 packages out-of-date; 1 packages too new
#> 
#> * sessionInfo()
#> 
#> R version 3.5.2 Patched (2019-02-12 r76095)
#> Platform: x86_64-pc-linux-gnu (64-bit)
#> Running under: Ubuntu 18.10
#> 
#> Matrix products: default
#> BLAS: /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.8.0
#> LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.8.0
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
#> other attached packages:
#> [1] BiocManager_1.30.5
#> 
#> loaded via a namespace (and not attached):
#>  [1] compiler_3.5.1  magrittr_1.5    tools_3.5.1     htmltools_0.3.6
#>  [5] yaml_2.2.0      Rcpp_1.0.1      stringi_1.4.3   rmarkdown_1.12 
#>  [9] knitr_1.22      stringr_1.4.0   xfun_0.6        digest_0.6.18  
#> [13] evaluate_0.13  
#> 
#> Bioconductor version '3.8'
#> 
#>   * 0 packages out-of-date
#>   * 1 packages too new
#> 
#> create a valid installation with
#> 
#>   BiocManager::install("BiocManager", update = TRUE, ask = FALSE)
#> 
#> more details: BiocManager::valid()$too_new, BiocManager::valid()$out_of_date
```

## More information

Please see the package vignette for more detailed information such as
changing Bioconductor version, offline use, and other advanced usage.

## Getting help

To report apparent bugs, create a minimal and reproducible example on
[github](https://github.com/Bioconductor/BiocManager/issues)
