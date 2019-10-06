library(testthat)
library(BiocManager)

options(BIOCMANAGER_CRANCHECK_BEHAVIOR = FALSE)
test_check("BiocManager")
options(BIOCMANAGER_CRANCHECK_BEHAVIOR = NULL)
