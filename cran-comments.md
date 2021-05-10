## Test environments
* local Ubuntu 20.10 (linux): R 3.6.3, R 4.0.5 patched, R-devel 4.2.0
* win-builder.r-project.org (windows): oldrel, release, devel
* appveyor (windows): bioc-devel, bioc-release, bioc-oldrel,
    oldrel, release, devel

## R CMD check results

0 errors | 0 warnings | 1 note

* checking package dependencies ... NOTE
Packages suggested but not available for checking:
  'BiocVersion', 'remotes', 'rmarkdown', 'curl'

`BiocVersion` is only available in Bioconductor.

## revdepcheck results

We checked 136 reverse dependencies (0 from CRAN + 136 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

https://github.com/LiNk-NY/BiocManagerRevDeps
