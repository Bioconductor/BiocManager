We checked for any major issues with `revdepcheck::revdep_check()` and
found none.

## Test environments

* local Ubuntu 22.04 (linux): R 4.2.2 patched, R 4.3.0 patched, R devel (r84486)
* win-builder.r-project.org (windows): R 4.2.3, R 4.3.0, R devel (r84521)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking package dependencies ... NOTE
Packages suggested but not available for checking:
  'BiocVersion', 'curl'

`BiocVersion` is only available in Bioconductor.

## revdepcheck results

We checked 177 reverse dependencies (0 from CRAN + 177 from Bioconductor),
comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
