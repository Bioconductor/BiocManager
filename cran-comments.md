Tested with `_R_CHECK_DEPENDS_ONLY=true` flag for Fedora.
We also checked for any major issues with `revdepcheck::revdep_check()` and
found none.

## Test environments
* local Ubuntu 20.10 (linux): R 4.0.5 patched, R 4.1.0 beta, R-devel 4.2.0
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

We checked 137 reverse dependencies (0 from CRAN + 137 from Bioconductor),
comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

https://github.com/LiNk-NY/BiocManagerRevDeps
