We checked for any major issues with `revdepcheck::revdep_check()` and
found none.

## Test environments
* local Ubuntu 22.04 (linux): R 4.1.3 patched, R 4.2.2 patched, R devel (r83801)
* R-Hub Service: Fedora Linux, R-devel, GCC; Debian Linux, R-release, GCC
* win-builder.r-project.org (windows): R 4.1.3 patched, R 4.2.2 patched, R
devel (r83857)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking package dependencies ... NOTE
Packages suggested but not available for checking:
  'BiocVersion', 'curl'

`BiocVersion` is only available in Bioconductor.

## revdepcheck results

We checked 169 reverse dependencies (0 from CRAN + 169 from Bioconductor),
comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
