We checked for any major issues with `revdepcheck::revdep_check()` and
found none.

## Test environments

* local Ubuntu 22.04 (linux): R 4.2.3, R 4.3.1, R devel (r84879)
* win-builder.r-project.org (windows): R 4.2.3, R 4.3.1, R devel (r84865)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking package dependencies ... NOTE
Packages suggested but not available for checking:
  'BiocVersion', 'curl'

`BiocVersion` is only available in Bioconductor.

## revdepcheck results

We checked 180 reverse dependencies (0 from CRAN + 180 from Bioconductor),
comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
