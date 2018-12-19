library(testthat)
library(BiocManager)

skip_if(getRversion() > package_version("3.5.0"),
{
    test_check("BiocManager")
})

