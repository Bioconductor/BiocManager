## Test environments
* local Ubuntu 18.04 LTS, R 3.5.1
* win-builder.r-project.org (release, devel)
* travis-ci (linux): R 3.5.1, R-devel
* appveyor-ci (win): R 3.5.1, R-devel

## R CMD check results

1 error (old-rel) | 0 warnings | 1 note

* checking package dependencies ... NOTE
Packages suggested but not available for checking:
  'BiocStyle' 'BiocVersion'

    These packages are available in Bioconductor.

* checking whether package ‘BiocManager’ can be
installed ... [0s/0s] ERROR
    Installation failed.

    This package is not compatible with r-oldrel
    (R 3.4.4) only R >= 3.5.0

## revdepcheck results

No strong reverse dependencies.

