## Test environments
* local Ubuntu 16.04 LTS, R 3.5.0
* win-builder.r-project.org (release, devel)

## R CMD check results

0 errors | 0 warnings | 2 notes

Note #1

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Marcel Ramos <marcel.ramos@roswellpark.org>’

New submission
```

This is the designated maintainer for the package.

Note #2

```
* checking package dependencies ... NOTE
Package suggested but not available for checking: 'BiocVersion'
```

This submission will enhance the use of `BiocVersion`, currently in
`Bioconductor`.

## Reviewer comments

> please ensure that you do not modify existing package libraries in your
examples/tests/vignette

We've checked all areas of the package for any modifications of existing
package libraries and did not find any.

Could you clarify what code modifies existing libraries? Thank you!

