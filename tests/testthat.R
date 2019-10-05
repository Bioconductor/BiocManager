library(testthat)
library(BiocManager)

if (!BiocManager:::.is_CRAN_check())
    test_check("BiocManager")
