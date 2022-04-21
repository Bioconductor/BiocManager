Tested on Fedora with `rhub::check_on_fedora()`.
We also checked for any major issues with `revdepcheck::revdep_check()` and
found none.

## Test environments
* local Ubuntu 20.10 (linux): R 4.0.5 patched, R 4.1.1 patched, R 4.2.0 beta
* win-builder.r-project.org (windows): R 4.1.3, R 4.2.0 RC, devel

## R CMD check results

0 errors | 0 warnings | 1 note

* checking package dependencies ... NOTE
Packages suggested but not available for checking:
  'BiocVersion'

`BiocVersion` is only available in Bioconductor.

## revdepcheck results

We checked 137 reverse dependencies (0 from CRAN + 137 from Bioconductor),
comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages

https://github.com/LiNk-NY/BiocManagerRevDeps
