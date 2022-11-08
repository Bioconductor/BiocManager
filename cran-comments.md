We tried to use `rhub::check_on_fedora()` / `rhub::check_on_debian()` but they
are not working, currently.

We also checked for any major issues with `revdepcheck::revdep_check()` and
found none.

## Test environments
* local Ubuntu 22.04 (linux): R 4.1.3 patched, R 4.2.1 patched, R devel 
* win-builder.r-project.org (windows): R 4.2.1 patched, devel

## R CMD check results

0 errors | 0 warnings | 1 note

* checking package dependencies ... NOTE
Packages suggested but not available for checking:
  'BiocVersion'

`BiocVersion` is only available in Bioconductor.

## revdepcheck results

We checked 163 reverse dependencies (0 from CRAN + 163 from Bioconductor),
comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
